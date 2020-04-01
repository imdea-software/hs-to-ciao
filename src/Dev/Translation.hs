module Dev.Translation where
    
import Data.Char (toUpper, toLower)
import Data.Text (splitOn, pack, unpack)
import Debug.Trace

import Control.Monad (mapM)
import Control.Monad.Reader (ask, runReader)

import Language.Ghc.Misc
import Dev.Environment
import Dev.CiaoSyn
import Dev.IDDictionary
    
import CoreSyn
import TyCoRep (Type(..))
import GhcPlugins
    
placeholderPred :: (CiaoMetaPred, CiaoEntry, CiaoPred)
placeholderPred = (CiaoMetaPred ("", []), CiaoEntry ("", []), EmptyPred)

translate :: [Target] -> [CoreBind] -> CiaoProgram
-- translate binds = do
--   res <- mapM translateBind binds
--   return (CiaoProgram res)
translate targets binds = CiaoProgram $ map (\bind -> runReader (translateBind bind) (Environment targets $ collectLetBinds $ bind)) binds
                  
translateBind :: CoreBind -> Env (CiaoMetaPred, CiaoEntry, CiaoPred)
translateBind (Rec list) = let (var, exprBind) = list !! 0 in
                           translateCoreBndr var exprBind
translateBind (NonRec var exprBind) = translateCoreBndr var exprBind
          
translateCoreBndr :: CoreBndr -> CoreExpr -> Env (CiaoMetaPred, CiaoEntry, CiaoPred)
translateCoreBndr var exprBind = do
  env <- ask
  let name = hsIDtoCiaoFunctorID [] $ showHsID env $
                                 trace (showSDocUnsafe . ppr $ var) $
                                 trace (showSDocUnsafe . ppr . varType $ var) $
                                 trace (show . funArityOfArguments . varType $ var) var
  let metapred = CiaoMetaPred (name, funArityOfArguments $ varType var)
  let entry = CiaoEntry (name, init $ hsTypeToCiaoEntryTypes $ varType var)
  if name !! 0 == '$' then
     return placeholderPred
  else
      let arglist = map (showHsID env) $ trace (show $ fst funStrippedArgs) (fst funStrippedArgs)
          funStrippedArgs = unfoldLam exprBind
          remainingExpr = snd funStrippedArgs in
      case remainingExpr of
        caseExpr@(Case _ _ _ _) ->
            do
              (varID, alts) <- fromCaseGetVarIDAndAlts caseExpr
              listOfFunctions <- splitCaseClauses arglist varID (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) alts
              -- ciaoBinds <- mapM letBindToCiao $ letBinds env
              return (metapred, entry, CPredF (CiaoPredF listOfFunctions))
        _ ->
            do
              translatedBody <- translateFunBody arglist remainingExpr
              ciaoBinds <- mapM letBindToCiao $ letBinds env
              return (metapred, entry, CPredF (CiaoPredF [CiaoFunction (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) translatedBody ciaoBinds]))
                     
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)

splitCaseClauses :: [String] -> CiaoId -> CiaoHead -> [CoreAlt] -> Env [CiaoFunction]
splitCaseClauses idlist varAlt ciaohead ((_, _, (Case (Var _) _ _ subCaseAlts)):restOfAlts) =
    do
      splittedClauses <- splitCaseClauses idlist varAlt ciaohead subCaseAlts 
      splittedClausesRest <- splitCaseClauses idlist varAlt ciaohead restOfAlts
      return $ reverse $ splittedClauses ++ splittedClausesRest
splitCaseClauses idlist varAlt ciaohead@(CiaoTerm _ args) (alt:restOfAlts) = do
  env <- ask
  ciaoBinds <- mapM letBindToCiao $ letBinds env
  caseSplitted <- splitCaseClauses idlist varAlt ciaohead restOfAlts
  fh <- funhead alt
  fb <- funbody alt
  return ([ CiaoFunction fh fb ciaoBinds ] ++ caseSplitted)
        where funbody = \(_, _, expr) -> translateFunBody idlist expr
              funhead :: CoreAlt -> Env CiaoHead
              funhead = \(altcon, conargs, _) ->
                          let posArg = findInArgList (CiaoId $ (hsIDtoCiaoVarID idlist) . show $ varAlt) args in
                          case posArg of
                              Nothing -> return ciaohead
                              (Just pos) ->
                                  do
                                    env <- ask
                                    case altcon of
                                      (LitAlt lit) -> return $ replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTermLit $ translateLit lit
                                      (DataAlt datacons) -> return $ replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showHsID env) datacons) $ map (\x -> CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoVarID idlist) . showHsID env) x) []) conargs
                                      DEFAULT -> return ciaohead
splitCaseClauses _ _ _ _ = return [] -- Placeholder for type-checker, there shouldn't be
                                     -- any heads other than functor + args

fromCaseGetVarIDAndAlts :: Expr b -> Env (CiaoId, [Alt b])
fromCaseGetVarIDAndAlts (Case (Var varID) _ _ altlist) = do
  env <- ask
  return (CiaoId $ showHsID env varID, altlist)
fromCaseGetVarIDAndAlts _ = return (CiaoId "BAD_ID", []) -- If this happens, there's something wrong
                         
ciaoOnlyIdsArgList :: [String] -> [CiaoArg]
ciaoOnlyIdsArgList list = map (CiaoArgId . CiaoId) $ map (hsIDtoCiaoVarID list) list

hsIDtoCiaoFunctorID :: [String] -> String -> String
hsIDtoCiaoFunctorID _ [] = []
hsIDtoCiaoFunctorID idlist str = let renamedID = (dollarFilter . map toLower . removeInvalid . idDictionary) (trace str str) in
                                 if (str `elem` idlist) then
                                     (toUpper . head $ renamedID):(tail renamedID)
                                 else
                                     renamedID
                          
hsIDtoCiaoVarID :: [String] -> String -> String
hsIDtoCiaoVarID _ [] = []
hsIDtoCiaoVarID _ str = let renamedID = (dollarFilter . map toLower . removeInvalid . idDictionary) (trace str str) in
                             (toUpper . head $ renamedID):(tail renamedID)

dollarFilter :: String -> String
dollarFilter str = let droppedDollar = dropWhile (/= '$') str in case droppedDollar of
                                                                   [] -> str
                                                                   _ -> droppedDollar
                                                         
removeInvalid :: String -> String
removeInvalid [] = []
removeInvalid "." = "."
removeInvalid str = map replaceChr str
    where replaceChr = (\x ->
                        case x of
                          '.' -> '_'
                          '\'' -> '_'
                          _ -> x)

hsTypeToCiaoEntryTypes :: Type -> [String]
hsTypeToCiaoEntryTypes generalType = map hsTypeToEntryEquivalent $ map unpack $ splitOn (pack " -> ") (pack $ showSDocUnsafe $ pprType $ generalType)

hsTypeToEntryEquivalent :: String -> String
hsTypeToEntryEquivalent str =
    case str of
      "[Int]" -> "{list, ground}"
      "Int" -> "int"
      _ -> ""
      
                       
funArityOfArguments :: Type -> [Int]
funArityOfArguments generalType = map typeArity $ fst $ splitFunTys generalType

typeArity :: Type -> Int 
typeArity (ForAllTy _ t) = typeArity t
typeArity (FunTy tx t) | isPredTy tx = typeArity t 
typeArity (FunTy _ t)    = 1 + typeArity t 
typeArity _              = 1 

tracepp :: Show a => String -> a -> a
tracepp string a = trace (string ++ show a) a
                           
translateFunBody :: [String] -> CoreExpr -> Env CiaoFunctionBody
translateFunBody idlist (Var x) = do
  env <- ask
  return $ CiaoFBTerm (CiaoId (((hsIDtoCiaoVarID idlist) . tracepp "showHsID" . showHsID env) x)) []
translateFunBody idlist (App x@(Var y) (Type _)) = do
  env <- ask
  case trace ("PATERROR?: " ++ show y) (showHsID env y) of
    "Control.Exception.Base.patError" -> return CiaoEmptyFB
    _ -> translateFunBody idlist x
translateFunBody idlist (App x (Type _)) = translateFunBody idlist x
translateFunBody _ (App (Var _) (Lit lit)) = return (CiaoFBLit $ translateLit lit)
translateFunBody idlist (Case app _ _ altlist) = let var = getCoreVarFromAppTree app in
                                                 case var of
                                                   Nothing -> return CiaoEmptyFB
                                                   (Just justvar) ->
                                                       do
                                                         env <- ask
                                                         collectedArgs <- collectArgsTree idlist app []
                                                         listOfAlts <- mapM (getTermFromCaseAlt idlist) $ altlist
                                                         return $ CiaoCaseFunCall (CiaoFBCall $ CiaoFunctionCall (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showHsID env) $ trace (show justvar) justvar) (reverse collectedArgs)) $ reverse listOfAlts
translateFunBody idlist (Let _ expr) = translateFunBody idlist expr
translateFunBody idlist expr =
    do
      (functor, isfunctor) <- getFunctorFromAppTree idlist expr
      let varexpr = getCoreVarFromAppTree expr
      case varexpr of
        Nothing -> return CiaoEmptyFB
        (Just justexpr) ->
            case isfunctor of
              True ->
                  do
                    collectedArgs <- collectArgsTree idlist expr []
                    return $ CiaoFBTerm functor $ trace (show $ reverse $ collectedArgs) (reverse $ collectedArgs)
              False -> let arity = typeArity $ varType (trace (show justexpr) justexpr) in
                       do
                         collectedArgs <- collectArgsTree idlist expr []
                         if length collectedArgs < arity - 1 then
                           return $ CiaoFBTerm functor $ trace (show $ reverse $ collectedArgs) (reverse $ collectedArgs)
                         else
                           return $ CiaoFBCall $ CiaoFunctionCall functor $ trace (show $ reverse $ collectedArgs) (reverse $ collectedArgs)
                                                  
getCoreVarFromAppTree :: CoreExpr -> Maybe Var
getCoreVarFromAppTree (Var x) = Just x
getCoreVarFromAppTree (Lit _) = Nothing
getCoreVarFromAppTree (App x _) = getCoreVarFromAppTree x
getCoreVarFromAppTree expr = error $ "Couldn't find the Var root of the App tree: " ++ (show expr)
                                          
getTermFromCaseAlt :: [String] -> CoreAlt -> Env (CiaoFunctionBody, CiaoFunctionBody)
getTermFromCaseAlt idlist (altcon, conargs, altExpr) = case altcon of
                                                 DataAlt datacons ->
                                                     do
                                                       env <- ask
                                                       translatedBody <- translateFunBody idlist altExpr
                                                       return (CiaoFBTerm (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showHsID env) datacons) $ map (\x -> CiaoFBTerm (CiaoId $ ((hsIDtoCiaoVarID idlist) . showHsID env) x) []) conargs, translatedBody)
                                                 LitAlt lit ->
                                                     do
                                                       translatedBody <- translateFunBody idlist altExpr
                                                       return (CiaoFBLit $ translateLit lit, translatedBody)
                                                 DEFAULT -> return (CiaoEmptyFB, CiaoEmptyFB) -- placeholder

getFunctorFromAppTree :: [String] -> CoreExpr -> Env (CiaoFunctor, Bool)
getFunctorFromAppTree idlist (Var x) =
    do
      env <- ask
      return (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showHsID env) x, isDataConWorkId x)
getFunctorFromAppTree idlist (App x _) = getFunctorFromAppTree idlist x
getFunctorFromAppTree _ _ = return (CiaoId "ERROR", False)

collectArgsTree :: [String] -> CoreExpr -> [CiaoFunctionBody] -> Env [CiaoFunctionBody]
collectArgsTree idlist (Var x) args =
    do
      env <- ask
      let argID = ((hsIDtoCiaoVarID idlist) . showHsID env) x
      if argID !! 0 == '$' then return args
      else return $ (CiaoFBTerm (CiaoId argID) []):args
collectArgsTree _ (App (Var y) (Type _)) args =
    do
      env <- ask
      case trace ("PATERROR? collectArgsTree: " ++ show y) (showHsID env y) of
        "Control.Exception.Base.patError" -> return []
        _ -> return args
collectArgsTree idlist (App (Var _) (Var y)) args =
    do
      env <- ask
      let argID = ((hsIDtoCiaoVarID idlist) . showHsID env) y
      if argID !! 0 == '$' then return args
      else return $ (CiaoFBTerm (CiaoId (((hsIDtoCiaoVarID idlist) . showHsID env) y)) []):args
collectArgsTree idlist (App x (Var y)) args =
    do
      env <- ask
      let argID = ((hsIDtoCiaoVarID idlist) . showHsID env) y
      if argID !! 0 == '$' then return args
      else
          do
            collectedArgs <- collectArgsTree idlist x args
            return $ (CiaoFBTerm (CiaoId argID) []):collectedArgs
collectArgsTree idlist (App x (Type _)) args = collectArgsTree idlist x args
collectArgsTree idlist (App (Var _) app) args =
    do
      translatedBody <- translateFunBody idlist app
      return (translatedBody:args)
collectArgsTree idlist (App x app) args =
    do
      translatedBody <- translateFunBody idlist app
      collectedArgs <- collectArgsTree idlist x args
      return (translatedBody:collectedArgs)
collectArgsTree _ (Type _) args = return args
collectArgsTree _ expr _ = error $ "Tree of nested App in function body has something other than Var and App: " ++ (show expr)
                           
clauseReturnSimpleVal :: CiaoHead -> CiaoFunctionBody -> Env CiaoPred
clauseReturnSimpleVal ciaohead fbody = do
  env <- ask
  ciaoBinds <- mapM letBindToCiao $ letBinds env
  return $ CPredF (CiaoPredF [CiaoFunction ciaohead fbody ciaoBinds])

replaceArgInHeadWith :: CiaoHead -> Int -> CiaoArg -> CiaoHead
replaceArgInHeadWith (CiaoTerm functor arglist) poshead arghead = CiaoTerm functor $ replace arglist poshead arghead
    where replace [] _ _ = []
          replace (_:xs) 0 arg = arg:xs
          replace (x:xs) pos arg = x:(replace xs (pos - 1) arg)
replaceArgInHeadWith defaulthead _ _ = defaulthead

                       
findInArgList :: CiaoId -> [CiaoArg] -> Maybe Int
findInArgList _ [] = Nothing
findInArgList ciaoid (y:ys) = Just $ go 0 ciaoid (y:ys)
    where
      go _ _ [] = -1
      go acc id' (x:xs) =
          case x of
            (CiaoArgTerm _) -> go (acc + 1) id' xs
            (CiaoArgId argid) -> if argid == id' then acc else go (acc + 1) id' xs

-- For now this just translates the Literal as its immediate
-- String representation; will probably have to change in the future
translateLit :: Literal -> CiaoLiteral
translateLit lit = CiaoLitStr $ filter (not . (`elem` "#")) $ (showSDocUnsafe . ppr) lit

collectLetBinds :: CoreBind -> [CoreExpr]
collectLetBinds (Rec list) = let (_, exprBind) = list !! 0 in
                         collectFromBndr exprBind    
collectLetBinds (NonRec _ exprBind) = collectFromBndr exprBind

collectFromBndr :: CoreExpr -> [CoreExpr]
collectFromBndr exprBind = go exprBind []
    where go :: CoreExpr -> [CoreExpr] -> [CoreExpr]
          go exprB listOfBinds =
              case exprB of
                letBind@(Let _ expr) -> go expr (letBind:listOfBinds)
                (App expr _) -> go expr listOfBinds
                (Lam _ expr) -> go expr listOfBinds
                (Case expr _ _ _) -> go expr listOfBinds
                (Cast _ _) -> error "We've got a Cast! Now I have to deal with them. Congrats!"
                (Tick _ _) -> error "We've got a Tick! Now I have to deal with them. Congrats!"
                _ -> listOfBinds
                   
letBindToCiao :: CoreExpr -> Env CiaoBind
letBindToCiao (Let bind _) =
    do
      env <- ask
      case bind of
        (NonRec var exprBind) -> let name = hsIDtoCiaoFunctorID [] $ showHsID env $ var in
                                 do
                                   translatedBody <- translateFunBody [] exprBind
                                   return $ CiaoBind (CiaoId name, translatedBody)
        (Rec list) -> let (var, exprBind) = list !! 0
                          name = hsIDtoCiaoFunctorID [] $ showHsID env $ var in
                      do
                        translatedBody <- translateFunBody [] exprBind
                        return $ CiaoBind (CiaoId name, translatedBody)
                               
letBindToCiao _ = error "letBindToCiao should only receive a Let; check if it's receiving something else!"
