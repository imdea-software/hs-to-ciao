module Dev.Translation where
    
import Data.Char (toUpper, toLower)
import Data.Text (splitOn, pack, unpack)
import Debug.Trace

import Control.Monad (mapM)
import Control.Monad.State.Lazy (get, put, runState)
--import Control.Monad.Reader (ask, runReader)

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
translate targets binds = CiaoProgram $ map (fst . (\bind -> runState (translateBind bind) (Environment targets (collectLetBinds bind) []))) binds
                  
translateBind :: CoreBind -> Env (CiaoMetaPred, CiaoEntry, CiaoPred)
translateBind (Rec list) = let (var, exprBind) = list !! 0 in
                           translateCoreBndr var exprBind
translateBind (NonRec var exprBind) = translateCoreBndr var exprBind
          
translateCoreBndr :: CoreBndr -> CoreExpr -> Env (CiaoMetaPred, CiaoEntry, CiaoPred)
translateCoreBndr var exprBind = do
  env <- get
  let name = hsIDtoCiaoFunctorID [] $ showHsID env $
                                 trace (showSDocUnsafe . ppr $ var) $
                                 trace (showSDocUnsafe . ppr . varType $ var) $
                                 trace (show . funArityOfArguments . varType $ var) var
  let metapred = CiaoMetaPred (name, funArityOfArguments $ varType var)
  let entry = CiaoEntry (name, init $ hsTypeToCiaoEntryTypes $ varType var)
  if name !! 0 == '$' then
     return placeholderPred
  else
      let arglist = trace (show $ fst funStrippedArgs) (fst funStrippedArgs)
          funStrippedArgs = unfoldLam exprBind
          remainingExpr = snd funStrippedArgs in
      case remainingExpr of
        caseExpr@(Case _ _ _ _) ->
            do
              put $ env {boundIds = arglist}
              (varID, alts) <- fromCaseGetVarIDAndAlts caseExpr
              listOfFunctions <- splitCaseClauses varID (CiaoTerm (CiaoId name) $ (ciaoOnlyIdsArgList . map (showSDocUnsafe . ppr)) arglist) alts
              return (metapred, entry, CPredF (CiaoPredF listOfFunctions))
        _ ->
            do
              put $ env {boundIds = arglist}
              translatedBody <- translateFunBody remainingExpr
              updatedEnv <- get -- Updated env to retrieve the new info
              ciaoBinds <- mapM letBindToCiao $ letBinds updatedEnv
              return (metapred, entry, CPredF (CiaoPredF [CiaoFunction (CiaoTerm (CiaoId name) $ (ciaoOnlyIdsArgList . map (showSDocUnsafe . ppr)) arglist) translatedBody ciaoBinds]))
                     
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)

splitCaseClauses :: CiaoId -> CiaoHead -> [CoreAlt] -> Env [CiaoFunction]
splitCaseClauses varAlt ciaohead ((_, _, (Case (Var _) _ _ subCaseAlts)):restOfAlts) =
    do
      splittedClauses <- splitCaseClauses varAlt ciaohead subCaseAlts 
      splittedClausesRest <- splitCaseClauses varAlt ciaohead restOfAlts
      return $ reverse $ splittedClauses ++ splittedClausesRest
splitCaseClauses varAlt ciaohead@(CiaoTerm _ args) (alt:restOfAlts) = do
  caseSplitted <- splitCaseClauses varAlt ciaohead restOfAlts
  fh <- funhead alt
  fb <- funbody alt
  env <- get
  ciaoBinds <- mapM letBindToCiao $ letBinds env
  return ([ CiaoFunction fh fb ciaoBinds ] ++ caseSplitted)
        where funbody = \(_, _, expr) -> translateFunBody expr
              funhead :: CoreAlt -> Env CiaoHead
              funhead = \(altcon, conargs, _) ->
                          let posArg = findInArgList (CiaoId $ (hsIDtoCiaoVarID . show) $ varAlt) args in
                          case posArg of
                              Nothing -> return ciaohead
                              (Just pos) ->
                                  do
                                    env <- get
                                    case altcon of
                                      (LitAlt lit) -> return $ replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTermLit $ translateLit lit
                                      (DataAlt datacons) -> return $ replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoFunctorID $ boundIds env) . showHsID env . dataConWorkId) datacons) $ map (\x -> CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoVarID . showHsID env) x)) []) conargs
                                      _ -> return ciaohead
splitCaseClauses _ _ _ = return [] -- Placeholder for type-checker, there shouldn't be
                                     -- any heads other than functor + args

fromCaseGetVarIDAndAlts :: Expr b -> Env (CiaoId, [Alt b])
fromCaseGetVarIDAndAlts (Case (Var varID) _ _ altlist) = do
  env <- get
  return (CiaoId $ showHsID env varID, altlist)
fromCaseGetVarIDAndAlts _ = return (CiaoId "BAD_ID", []) -- If this happens, there's something wrong
                         
ciaoOnlyIdsArgList :: [String] -> [CiaoArg]
ciaoOnlyIdsArgList list = map (CiaoArgId . CiaoId) $ map hsIDtoCiaoVarID list

hsIDtoCiaoFunctorID :: [Id] -> String -> String
hsIDtoCiaoFunctorID _ [] = []
hsIDtoCiaoFunctorID hsIds str = let renamedID = (dollarFilter . map toLower . removeInvalid . idDictionary) (trace str str)
                                    idlist = map (showSDocUnsafe . ppr) hsIds in
                                 if (str `elem` idlist) then
                                     (toUpper . head $ renamedID):(tail renamedID)
                                 else
                                     renamedID
                          
hsIDtoCiaoVarID :: String -> String
hsIDtoCiaoVarID [] = []
hsIDtoCiaoVarID str = let renamedID = (dollarFilter . map toLower . removeInvalid . idDictionary) (trace str str) in
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
tracepp string a = trace (string ++ " " ++ show a) a
                           
translateFunBody :: CoreExpr -> Env CiaoFunctionBody
translateFunBody (Var x) = do
  env <- get
  return $ CiaoFBTerm (CiaoId ((hsIDtoCiaoVarID . tracepp "showHsID" . showHsID env) x)) []
translateFunBody (App x (Type _)) = translateFunBody x
translateFunBody (App (Var _) (Lit lit)) = return (CiaoFBLit $ translateLit lit)
translateFunBody (Case app _ _ altlist) = let var = getCoreVarFromAppTree app in
                                                 case var of
                                                   Nothing -> return CiaoEmptyFB
                                                   (Just justvar) ->
                                                       case show justvar of
                                                         "control_exception_base_paterror" -> return CiaoEmptyFB
                                                         _ ->
                                                             do
                                                               env <- get
                                                               collectedArgs <- collectArgsTree app []
                                                               listOfAlts <- mapM getTermFromCaseAlt altlist
                                                               return $ CiaoCaseFunCall (CiaoFBCall $ CiaoFunctionCall (CiaoId $ ((hsIDtoCiaoFunctorID $ boundIds env) . showHsID env) $ trace (show justvar) justvar) (reverse collectedArgs)) $ reverse listOfAlts
translateFunBody (Let _ expr) = translateFunBody expr
translateFunBody expr =
    do
      (functor, isfunctor) <- getFunctorFromAppTree expr
      case show functor of
        "control_exception_base_paterror" -> return CiaoEmptyFB
        _ ->
            let varexpr = getCoreVarFromAppTree expr in
            case varexpr of
              Nothing -> return CiaoEmptyFB
              (Just justexpr) ->
                  case isfunctor of
                    True ->
                        do
                          collectedArgs <- collectArgsTree expr []
                          return $ CiaoFBTerm functor $ trace ("COLLECTED ARGS #1: " ++ (show $ reverse $ collectedArgs)) (drop 1 $ reverse $ collectedArgs)
                    False -> let arity = typeArity $ varType (trace (show justexpr) justexpr) in
                        do
                          collectedArgs <- collectArgsTree expr []
                          if length collectedArgs < arity - 1 then
                              return $ CiaoFBTerm functor $ trace ("COLLECTED ARGS #2: " ++ (show $ reverse $ collectedArgs)) (reverse $ collectedArgs)
                          else
                              return $ CiaoFBCall $ CiaoFunctionCall functor $ trace ("COLLECTED ARGS #3: " ++ (show $ reverse $ collectedArgs)) (reverse $ collectedArgs)
                                                  
getCoreVarFromAppTree :: CoreExpr -> Maybe Var
getCoreVarFromAppTree (Var x) = Just x
getCoreVarFromAppTree (Lit _) = Nothing
getCoreVarFromAppTree (App x _) = getCoreVarFromAppTree x
getCoreVarFromAppTree expr = error $ "Couldn't find the Var root of the App tree: " ++ (show expr)
                                          
getTermFromCaseAlt :: CoreAlt -> Env (CiaoFunctionBody, CiaoFunctionBody)
getTermFromCaseAlt (altcon, conargs, altExpr) = case altcon of
                                                 DataAlt datacons ->
                                                     do
                                                       env <- get
                                                       translatedBody <- translateFunBody altExpr
                                                       return (CiaoFBTerm (CiaoId $ ((hsIDtoCiaoFunctorID $ boundIds env) . showHsID env . dataConWorkId) datacons) $ map (\x -> CiaoFBTerm (CiaoId $ (hsIDtoCiaoVarID . showHsID env) x) []) conargs, translatedBody)
                                                 LitAlt lit ->
                                                     do
                                                       translatedBody <- translateFunBody altExpr
                                                       return (CiaoFBLit $ translateLit lit, translatedBody)
                                                 DEFAULT -> return (CiaoEmptyFB, CiaoEmptyFB) -- placeholder

getFunctorFromAppTree :: CoreExpr -> Env (CiaoFunctor, Bool)
getFunctorFromAppTree (Var x) =
    do
      env <- get
      return (CiaoId $ ((hsIDtoCiaoFunctorID $ boundIds env) . showHsID env) x, isDataConWorkId x)
getFunctorFromAppTree (App x _) = getFunctorFromAppTree x
getFunctorFromAppTree _ = return (CiaoId "ERROR", False)

collectArgsTree :: CoreExpr -> [CiaoFunctionBody] -> Env [CiaoFunctionBody]
collectArgsTree (Var x) args =
    do
      env <- get
      let argID = (hsIDtoCiaoVarID . showHsID env) x
      if argID !! 0 == '$' then return args
      else return $ (CiaoFBTerm (CiaoId argID) []):args
collectArgsTree (App (Var _) (Var y)) args =
    do
      env <- get
      let argID = (hsIDtoCiaoVarID . showHsID env) y
      if argID !! 0 == '$' then return args
      else return $ (CiaoFBTerm (CiaoId ((hsIDtoCiaoVarID . showHsID env) y)) []):args
collectArgsTree (App x (Var y)) args =
    do
      env <- get
      let argID = (hsIDtoCiaoVarID . showHsID env) y
      if argID !! 0 == '$' then return args
      else
          do
            collectedArgs <- collectArgsTree x args
            return $ (CiaoFBTerm (CiaoId argID) []):collectedArgs
collectArgsTree (App x (Type _)) args = collectArgsTree x args
collectArgsTree (App (Var _) app) args =
    do
      translatedBody <- translateFunBody app
      return (translatedBody:args)
collectArgsTree (App x app) args =
    do
      translatedBody <- translateFunBody app
      collectedArgs <- collectArgsTree x args
      return (translatedBody:collectedArgs)
collectArgsTree (Type _) args = return args
collectArgsTree expr _ = error $ "Tree of nested App in function body has something other than Var and App: " ++ (show expr)
                           
clauseReturnSimpleVal :: CiaoHead -> CiaoFunctionBody -> Env CiaoPred
clauseReturnSimpleVal ciaohead fbody = do
  env <- get
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
      case bind of
        (NonRec var exprBind) ->
            do
              --put $ env { boundIds = [] }
              translatedBody <- translateFunBody exprBind
              env <- get
              let name = hsIDtoCiaoVarID $ showHsID env $ var
              return $ CiaoBind (CiaoId name, translatedBody)
        (Rec list) -> let (var, exprBind) = list !! 0 in
                      do
                        --put $ env { boundIds = [] }
                        translatedBody <- translateFunBody exprBind
                        env <- get
                        let name = hsIDtoCiaoVarID $ showHsID env $ var
                        return $ CiaoBind (CiaoId name, translatedBody)
letBindToCiao _ = error "letBindToCiao should only receive a Let; check if it's receiving something else!"
