module Dev.Translation where
    
import Data.Char (toUpper, toLower)
import Debug.Trace

import Control.Monad (mapM)
import Control.Monad.Reader

import Language.Ghc.Misc
import Dev.CiaoSyn
import Dev.IDDictionary
    
import CoreSyn
import TyCoRep (Type(..))
import GhcPlugins
    
placeholderPred :: (CiaoMetaPred, CiaoPred)
placeholderPred = (CiaoMetaPred ("", []), EmptyPred)

translate :: [CoreBind] -> Reader Environment CiaoProgram
translate binds = do
  res <- mapM translateBind binds
  return (CiaoProgram res)
                  
translateBind :: CoreBind -> Reader Environment (CiaoMetaPred, CiaoPred)

translateBind bind =
    case bind of
      Rec list -> let (var, exprBind) = list !! 0 in translateCoreBndr var exprBind
      NonRec var exprBind -> translateCoreBndr var exprBind
          
translateCoreBndr :: CoreBndr -> CoreExpr -> Reader Environment (CiaoMetaPred, CiaoPred)
translateCoreBndr var exprBind = do
  env <- ask
  let name = hsIDtoCiaoFunctorID [] $ showHsID env $
                                 trace (showSDocUnsafe . ppr $ var) $
                                 trace (showSDocUnsafe . ppr . varType $ var) $
                                 trace (show . funArityOfArguments . varType $ var) var
  let metapred = CiaoMetaPred (name, funArityOfArguments $ varType var)
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
              return (metapred, CPredF $ CiaoPredF $ listOfFunctions)
        _ ->
            do
              translatedBody <- translateFunBody arglist remainingExpr
              return (metapred, CPredF $ CiaoPredF $ [CiaoFunction (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) $ translatedBody]) 
          
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)

splitCaseClauses :: [String] -> CiaoId -> CiaoHead -> [CoreAlt] -> Reader Environment [CiaoFunction]
splitCaseClauses idlist varAlt ciaohead ((_, _, (Case (Var _) _ _ subCaseAlts)):restOfAlts) =
    do
      splittedClauses <- splitCaseClauses idlist varAlt ciaohead subCaseAlts 
      splittedClausesRest <- splitCaseClauses idlist varAlt ciaohead restOfAlts
      return $ reverse $ splittedClauses ++ splittedClausesRest
splitCaseClauses idlist varAlt ciaohead@(CiaoTerm _ args) (alt:restOfAlts) = do
  caseSplitted <- splitCaseClauses idlist varAlt ciaohead restOfAlts
  fh <- funhead alt
  fb <- funbody alt
  return ([ CiaoFunction fh fb ] ++ caseSplitted)
        where funbody = \(_, _, expr) -> translateFunBody idlist expr
              funhead :: CoreAlt -> Reader Environment CiaoHead
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

fromCaseGetVarIDAndAlts :: Expr b -> Reader Environment (CiaoId, [Alt b])
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
                          
funArityOfArguments :: Type -> [Int]
funArityOfArguments generalType = map typeArity $ fst $ splitFunTys generalType

typeArity :: Type -> Int 
typeArity (ForAllTy _ t) = typeArity t
typeArity (FunTy tx t) | isPredTy tx = typeArity t 
typeArity (FunTy _ t)    = 1 + typeArity t 
typeArity _              = 1 

tracepp :: Show a => String -> a -> a
tracepp string a = trace (string ++ show a) a
                           
translateFunBody :: [String] -> CoreExpr -> Reader Environment CiaoFunctionBody
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
                                          
getTermFromCaseAlt :: [String] -> CoreAlt -> Reader Environment (CiaoFunctionBody, CiaoFunctionBody)
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

getFunctorFromAppTree :: [String] -> CoreExpr -> Reader Environment (CiaoFunctor, Bool)
getFunctorFromAppTree idlist (Var x) =
    do
      env <- ask
      return (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showHsID env) x, isDataConWorkId x)
getFunctorFromAppTree idlist (App x _) = getFunctorFromAppTree idlist x
getFunctorFromAppTree _ _ = return (CiaoId "ERROR", False)

collectArgsTree :: [String] -> CoreExpr -> [CiaoFunctionBody] -> Reader Environment [CiaoFunctionBody]
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
                           
clauseReturnSimpleVal :: CiaoHead -> CiaoFunctionBody -> CiaoPred
clauseReturnSimpleVal ciaohead fbody = CPredF $ CiaoPredF [CiaoFunction ciaohead fbody]

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
