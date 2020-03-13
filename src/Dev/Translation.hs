module Dev.Translation where

import Data.Char (toUpper, toLower)
import Debug.Trace
    
import Language.Ghc.Misc (showQualified)
import Dev.CiaoSyn
import Dev.IDDictionary
    
import CoreSyn
import TyCoRep (Type(..))
import GhcPlugins

-- Take into special consideration Haskell functions that return Bool

placeholderPred :: (CiaoMetaPred, CiaoPred)
placeholderPred = (CiaoMetaPred ("", []), EmptyPred)

translate :: [CoreBind] -> CiaoProgram
translate binds = CiaoProgram $ map translateBind binds
                  
translateBind :: CoreBind -> (CiaoMetaPred, CiaoPred)
translateBind bind =
    case bind of
      Rec list -> let (var, exprBind) = list !! 0 in translateCoreBndr var exprBind
      NonRec var exprBind -> translateCoreBndr var exprBind
          
translateCoreBndr :: CoreBndr -> CoreExpr -> (CiaoMetaPred, CiaoPred)
translateCoreBndr var exprBind =
    if name !! 0 == '$' then placeholderPred else
              let arglist = map show $ trace (show $ fst funStrippedArgs) (fst funStrippedArgs);
                  funStrippedArgs = unfoldLam exprBind;
                  remainingExpr = snd funStrippedArgs in
              case remainingExpr of
                caseExpr@(Case _ _ _ _) -> (metapred, CPredF $ CiaoPredF $ splitCaseClauses arglist varID (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) alts)
                    where (varID, alts) = fromCaseGetVarIDAndAlts caseExpr
                                --fbody = CiaoFBTerm (CiaoId "[]") []
                _ -> (metapred, CPredF $ CiaoPredF $ [CiaoFunction (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) $ translateFunBody arglist remainingExpr])
        where metapred = CiaoMetaPred (name, funArityOfArguments $ varType var)
              name = hsIDtoCiaoFunctorID [] $
                     show (trace (showSDocUnsafe . ppr $ var) $
                           trace (showSDocUnsafe . ppr . varType $ var) $
                           trace (show . funArityOfArguments . varType $ var) var)
              
      --_ -> EmptyPred
          
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)

splitCaseClauses :: [String] -> CiaoId -> CiaoHead -> [CoreAlt] -> [CiaoFunction]
splitCaseClauses idlist varAlt ciaohead ((_, _, (Case (Var _) _ _ subCaseAlts)):restOfAlts) = reverse $ splitCaseClauses idlist varAlt ciaohead subCaseAlts ++ splitCaseClauses idlist varAlt ciaohead restOfAlts
splitCaseClauses idlist varAlt ciaohead@(CiaoTerm _ args) (alt:restOfAlts) = 
    [ CiaoFunction (funhead alt) (funbody alt) ] ++ splitCaseClauses idlist varAlt ciaohead restOfAlts
        where funbody = \(_, _, expr) -> translateFunBody idlist expr
              funhead :: CoreAlt -> CiaoHead
              funhead = \(altcon, conargs, _) ->
                               let posArg = findInArgList (CiaoId $ (hsIDtoCiaoVarID idlist) . show $ varAlt) args in
                               case posArg of
                                 Nothing -> ciaohead
                                 (Just pos) -> 
                                     case altcon of
                                       (LitAlt lit) -> replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTermLit $ translateLit lit
                                       (DataAlt datacons) -> replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showQualified) datacons) $ map (\x -> CiaoArgTerm $ CiaoTerm (CiaoId $ ((hsIDtoCiaoVarID idlist) . showQualified) x) []) conargs
                                       DEFAULT -> ciaohead
splitCaseClauses _ _ _ _ = [] -- Placeholder for type-checker, there shouldn't be
                          -- any heads other than functor + args

fromCaseGetVarIDAndAlts :: Expr b -> (CiaoId, [Alt b])
fromCaseGetVarIDAndAlts (Case (Var varID) _ _ altlist) = (CiaoId $ show varID, altlist)
fromCaseGetVarIDAndAlts _ = (CiaoId "BAD_ID", []) -- If this happens, there's something wrong
                         
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
                           
translateFunBody :: [String] -> CoreExpr -> CiaoFunctionBody
translateFunBody idlist (Var x) = CiaoFBTerm (CiaoId (((hsIDtoCiaoVarID idlist) . tracepp "showQualified" . showQualified) x)) []
translateFunBody idlist (App x (Type _)) = translateFunBody idlist x
translateFunBody _ (App (Var _) (Lit lit)) = CiaoFBLit $ translateLit lit
translateFunBody idlist (Case app _ _ altlist) = CiaoCaseFunCall (CiaoFBCall $ CiaoFunctionCall (CiaoId $ ((hsIDtoCiaoVarID idlist) . showQualified) $ trace (show $ getCoreVarFromAppTree app) (getCoreVarFromAppTree app)) (reverse $ collectArgsTree idlist app [])) $ (reverse . map (getTermFromCaseAlt idlist)) altlist
translateFunBody idlist expr = let (functor, isfunctor) = getFunctorFromAppTree idlist expr in
                        case isfunctor of
                          True -> CiaoFBTerm functor $ trace (show $ reverse $ collectArgsTree idlist expr []) (reverse $ collectArgsTree idlist expr [])
                          False -> let arity = typeArity $ varType (trace (show $ getCoreVarFromAppTree expr) $ getCoreVarFromAppTree expr); listOfArgs = collectArgsTree idlist expr [] in
                                   if length listOfArgs < arity - 1 then
                                       CiaoFBTerm functor $ trace (show $ reverse $ collectArgsTree idlist expr []) (reverse $ collectArgsTree idlist expr [])
                                   else
                                       CiaoFBCall $ CiaoFunctionCall functor $ trace (show $ reverse $ collectArgsTree idlist expr []) (reverse $ collectArgsTree idlist expr [])
                                                  
getCoreVarFromAppTree :: CoreExpr -> Var
getCoreVarFromAppTree (Var x) = x
getCoreVarFromAppTree (App x _) = getCoreVarFromAppTree x
getCoreVarFromAppTree expr = error $ "Couldn't find the Var root of the App tree: " ++ (show expr)
                                          
getTermFromCaseAlt :: [String] -> CoreAlt -> (CiaoFunctionBody, CiaoFunctionBody)
getTermFromCaseAlt idlist (altcon, conargs, altExpr) = case altcon of
                                                 DataAlt datacons -> (CiaoFBTerm (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showQualified) datacons) $ map (\x -> CiaoFBTerm (CiaoId $ ((hsIDtoCiaoVarID idlist) . show) x) []) conargs, translateFunBody idlist altExpr)
                                                 LitAlt lit -> (CiaoFBLit $ translateLit lit, translateFunBody idlist altExpr)
                                                 DEFAULT -> (CiaoEmptyFB, CiaoEmptyFB) -- placeholder

getFunctorFromAppTree :: [String] -> CoreExpr -> (CiaoFunctor, Bool)
getFunctorFromAppTree idlist (Var x) = (CiaoId $ ((hsIDtoCiaoFunctorID idlist) . showQualified) x, isDataConWorkId x)
getFunctorFromAppTree idlist (App x _) = getFunctorFromAppTree idlist x
getFunctorFromAppTree _ _ = (CiaoId "ERROR", False)

collectArgsTree :: [String] -> CoreExpr -> [CiaoFunctionBody] -> [CiaoFunctionBody]
collectArgsTree idlist (Var x) args = let argID = ((hsIDtoCiaoVarID idlist) . showQualified) x in
                                      if argID !! 0 == '$' then args
                                      else (CiaoFBTerm (CiaoId argID) []):args
collectArgsTree idlist (App (Var _) (Var y)) args = let argID = ((hsIDtoCiaoVarID idlist) . showQualified) y in
                                                    if argID !! 0 == '$' then args
                                                    else (CiaoFBTerm (CiaoId (((hsIDtoCiaoVarID idlist) . showQualified) y)) []):args
collectArgsTree idlist (App x (Var y)) args = let argID = ((hsIDtoCiaoVarID idlist) . showQualified) y in
                                              if argID !! 0 == '$' then args
                                              else (CiaoFBTerm (CiaoId argID) []):(collectArgsTree idlist x args)
collectArgsTree _ (App (Var _) (Type  _)) args = args
collectArgsTree idlist (App x (Type  _)) args = collectArgsTree idlist x args
collectArgsTree idlist (App (Var _) app) args = (translateFunBody idlist app):args
collectArgsTree idlist (App x app) args = (translateFunBody idlist app):(collectArgsTree idlist x args)
collectArgsTree _ (Type _) args = args
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
