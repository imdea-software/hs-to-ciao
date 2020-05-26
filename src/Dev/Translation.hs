module Dev.Translation where
    
import Data.Char (toUpper, toLower, isLower)
import Data.Text (splitOn, pack, unpack)
import Debug.Trace
import Data.List
    
import Control.Monad (mapM)
import Control.Monad.State.Strict (get, put, runState)
--import Control.Monad.Reader (ask, runReader)

import Language.Ghc.Misc
import Dev.Environment
import Dev.CiaoSyn
import Dev.IDDictionary
    
import CoreSyn
import TyCoRep (Type(..))
import GhcPlugins
    
placeholderFunctor :: CiaoFunctor
placeholderFunctor = CiaoFunctor { functorName = CiaoId ""
                                 , functorArity = 0
                                 , functorHsType = undefined -- ugly, but this shouldn't ever be accessed anyways
                                 , functorMetaPred = CiaoMetaPred ("", [])
                                 , functorEntry = CiaoEntry ("", [])
                                 , functorPredDefinition = EmptyPred
                                 , functorSubfunctorIds = [] }

translate :: [Target] -> [CoreBind] -> CiaoProgram
-- translate binds = do
--   res <- mapM translateBind binds
--   return (CiaoProgram res)
translate targets binds = CiaoProgram $ removePlaceholders $ map (fst . (\bind -> runState (translateBind bind) (Environment targets (collectLetBinds bind) [] []))) binds
    where removePlaceholders = filter ((/=(CiaoId "")) . functorName)
                          
translateBind :: CoreBind -> Env CiaoFunctor
translateBind (Rec list) = let (var, exprBind) = list !! 0 in
                           translateCoreBndr var exprBind
translateBind (NonRec var exprBind) = translateCoreBndr var exprBind
          
translateCoreBndr :: CoreBndr -> CoreExpr -> Env CiaoFunctor
translateCoreBndr var exprBind = do
  env <- get
  let name = hsIDtoCiaoFunctorID [] $ showHsID env $
                                 trace (showSDocUnsafe . ppr $ var) $
                                 trace (showSDocUnsafe . ppr . varType $ var) $
                                 trace (show . funArityOfArguments . varType $ var) var
  let argsArities = funArityOfArguments $ varType var
  let arity = length $ argsArities
  let metapred = CiaoMetaPred (name, argsArities)
  let entry = CiaoEntry (name, init $ hsTypeToCiaoEntryTypes $ varType var)
  if name !! 0 == '$' then
     return placeholderFunctor
  else
      let arglist = trace (show $ fst funStrippedArgs) (fst funStrippedArgs)
          funStrippedArgs = unfoldLam exprBind
          remainingExpr = snd funStrippedArgs
          noLetRemainingExpr = case trace ("Expression perhaps with Let: " ++ (show remainingExpr)) remainingExpr of
                                 (Let _ expr) -> expr
                                 _ -> remainingExpr
      in      
      case trace ("Now the expression shouldn't be a Let: " ++ (show noLetRemainingExpr)) noLetRemainingExpr of
        caseExpr@(Case _ _ _ _) ->
            do
              put $ env {boundArgs = trace ("ARGLIST: " ++ show arglist) arglist}
              (varID, alts) <- fromCaseGetVarIDAndAlts caseExpr
              listOfFunctions <- splitCaseClauses varID (CiaoTerm (CiaoId name) $ (ciaoOnlyIdsArgList . map (showSDocUnsafe . ppr)) arglist) alts
              updatedEnv <- get -- Updated env to retrieve the new info
              let predDefinition = CPredF (CiaoPredF listOfFunctions)
              return CiaoFunctor { functorName = (CiaoId name)
                                 , functorArity = arity
                                 , functorHsType = splitFunTys $ varType var
                                 , functorMetaPred = metapred
                                 , functorEntry = entry
                                 , functorPredDefinition = predDefinition
                                 , functorSubfunctorIds = fromEnvGetSubfunctorIds updatedEnv }
        _ ->
            do
              put $ env {boundArgs = trace ("ARGLIST: " ++ show arglist) arglist}
              translatedBody <- translateFunBody remainingExpr
              updatedEnv <- get -- Updated env to retrieve the new info
              ciaoBinds <- mapM letBindToCiao $ letBinds updatedEnv
              let predDefinition = CPredF (CiaoPredF [CiaoFunction (CiaoTerm (CiaoId name) $ (ciaoOnlyIdsArgList . map (showSDocUnsafe . ppr)) arglist) translatedBody ciaoBinds])
              return CiaoFunctor { functorName = (CiaoId name)
                                 , functorArity = arity
                                 , functorHsType = splitFunTys $ varType var
                                 , functorMetaPred = metapred
                                 , functorEntry = entry
                                 , functorPredDefinition = predDefinition
                                 , functorSubfunctorIds = fromEnvGetSubfunctorIds updatedEnv }
                     
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
                                      (DataAlt datacons) -> ciaoTerm >>= (return . replaceArgInHeadWith ciaohead pos)
                                          where ciaoTerm = do
                                                  cAList <- ciaoArgList
                                                  put $ env {usedIdsInBody = (dataConWorkId datacons):(usedIdsInBody env)}
                                                  let cTerm = CiaoTerm (CiaoId $ ((hsIDtoCiaoFunctorID $ boundArgs env) . showHsID env . dataConWorkId) datacons) $ cAList
                                                  return $ CiaoArgTerm cTerm
                                                ciaoArgList = sequence $ map (\x -> (put $ env {usedIdsInBody = x:(usedIdsInBody env)} :: Env ()) >>
                                                                                    (return $ CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoVarID . showHsID env) x)) [])) conargs
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
                                                               put $ env {usedIdsInBody = justvar:(usedIdsInBody env)}
                                                               return $ CiaoCaseFunCall (CiaoFBCall $ CiaoFunctionCall (CiaoId $ ((hsIDtoCiaoFunctorID $ boundArgs env) . showHsID env) $ trace ("JUSTVAR: " ++ show justvar) justvar) (reverse collectedArgs)) $ reverse listOfAlts
translateFunBody (Let _ expr) = translateFunBody expr
translateFunBody maybeVoidExpr =
    case maybeVoidExpr of
      (App _ (Var x)) -> case trace ("THIS GUY COULD BE VOID#: " ++ show x) (show x) of
                           "void#" -> trace ("RETURNING EMPTY BODY: ") $ return CiaoEmptyFB
                           _ -> translateFunBodyAux maybeVoidExpr
      _ -> translateFunBodyAux maybeVoidExpr
    where translateFunBodyAux = (\expr ->
              do
                (functor, isfunctor) <- getFunctorFromAppTree expr
                case show functor of
                  "control_exception_base_paterror" -> return CiaoEmptyFB
                  _ -> let varexpr = getCoreVarFromAppTree expr in
                       case varexpr of
                         Nothing -> return CiaoEmptyFB
                         (Just justexpr) ->
                             case isfunctor of
                               True ->
                                   do
                                     collectedArgs <- collectArgsTree expr []
                                     return $ CiaoFBTerm functor $ trace ("COLLECTED ARGS #1: " ++ (show collectedArgs)) (drop 1 $ reverse collectedArgs)
                               False -> let arity = typeArity $ varType (trace (show justexpr) justexpr) in
                                   do
                                     tmpColArgs <- collectArgsTree expr []
                                     let reversedArgs = reverse tmpColArgs
                                     let hd = head reversedArgs
                                     let tl = tail reversedArgs
                                     let collectedArgs = if show hd == (toUpper . head $ show functor):(tail $ show functor) then tl else hd:tl
                                     if length collectedArgs < arity - 1 then
                                         return $ CiaoFBTerm functor $ trace ("COLLECTED ARGS #2: " ++ (show collectedArgs)) collectedArgs
                                     else
                                         return $ CiaoFBCall $ CiaoFunctionCall functor $ trace ("COLLECTED ARGS #3: " ++ (show collectedArgs)) collectedArgs)
                                                  
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
                                                       put $ env {usedIdsInBody = (dataConWorkId datacons):(usedIdsInBody env)}
                                                       return (CiaoFBTerm (CiaoId $ ((hsIDtoCiaoFunctorID $ boundArgs env) . showHsID env . dataConWorkId) datacons) $ map (\x -> CiaoFBTerm (CiaoId $ (hsIDtoCiaoVarID . showHsID env) x) []) conargs, translatedBody)
                                                 LitAlt lit ->
                                                     do
                                                       translatedBody <- translateFunBody altExpr
                                                       return (CiaoFBLit $ translateLit lit, translatedBody)
                                                 DEFAULT -> return (CiaoEmptyFB, CiaoEmptyFB) -- placeholder

getFunctorFromAppTree :: CoreExpr -> Env (CiaoFunctorName, Bool)
getFunctorFromAppTree (Var x) =
    do
      env <- get
      put $ env {usedIdsInBody = x:(usedIdsInBody env)}
      return (CiaoId $ ((hsIDtoCiaoFunctorID $ boundArgs env) . showHsID env) x, isDataConWorkId x)
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

fromEnvGetSubfunctorIds :: Environment -> [String]
fromEnvGetSubfunctorIds env = filter (\(x:_) -> isLower x) $ trace ("SUBFUNCTOR IDs BEFORE FILTER:\nID: " ++ (intercalate "\nID: " $ map (hsIDtoCiaoFunctorID (trace ("USED IDS IN BODY: " ++ show ids) ids) . showHsID env) $ ids)) (map (hsIDtoCiaoFunctorID ids . showHsID env) $ ids)
    where ids = usedIdsInBody env                   
                   
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
              translatedBody <- translateFunBody exprBind
              env <- get
              let name = hsIDtoCiaoVarID $ showHsID env $ var
              return $ CiaoBind (CiaoId name, translatedBody)
        (Rec list) -> let (var, exprBind) = list !! 0 in
                      do
                        translatedBody <- translateFunBody exprBind
                        env <- get
                        let name = hsIDtoCiaoVarID $ showHsID env $ var
                        return $ CiaoBind (CiaoId name, translatedBody)
letBindToCiao _ = error "letBindToCiao should only receive a Let; check if it's receiving something else!"
