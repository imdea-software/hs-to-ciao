module Dev.Translation where

import Data.Char (toUpper)
import Debug.Trace
    
import Language.Ghc.Misc ()
import Dev.CiaoSyn
    
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
    -- let name = show (trace (showSDocUnsafe . ppr $ var) $
          --                  trace (showSDocUnsafe . ppr . varType $ var) $
          --                  trace (show . funArityOfArguments . varType $ var) var) in
          
          --(metapred, clauseReturnSimpleVal (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) fbody)
    if name == "$trModule" then placeholderPred else
              let arglist = map show $ trace (show $ fst funStrippedArgs) (fst funStrippedArgs);
                  funStrippedArgs = unfoldLam exprBind;
                  remainingExpr = snd funStrippedArgs in
              case remainingExpr of
                caseExpr@(Case _ _ _ _) -> (metapred, CPredF $ CiaoPredF $ splitCaseClauses varID (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) alts)
                    where (varID, alts) = fromCaseGetVarIDAndAlts caseExpr
                                --fbody = CiaoFBTerm (CiaoId "[]") []
                _ -> (metapred, CPredF $ CiaoPredF $ [CiaoFunction (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) $ translateFunBody remainingExpr])
        where metapred = CiaoMetaPred (name, funArityOfArguments $ varType var)
              name = hsIDtoCiaoFunctorID $
                     show (trace (showSDocUnsafe . ppr $ var) $
                           trace (showSDocUnsafe . ppr . varType $ var) $
                           trace (show . funArityOfArguments . varType $ var) var)
              
      --_ -> EmptyPred
          
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)

splitCaseClauses :: CiaoId -> CiaoHead -> [CoreAlt] -> [CiaoFunction]
splitCaseClauses varAlt ciaohead ((_, _, (Case (Var _) _ _ subCaseAlts)):restOfAlts) = reverse $ splitCaseClauses varAlt ciaohead subCaseAlts ++ splitCaseClauses varAlt ciaohead restOfAlts
splitCaseClauses varAlt ciaohead@(CiaoTerm _ args) (alt:restOfAlts) = trace ("HOLA2") $
    [ CiaoFunction (funhead alt) (funbody alt) ] ++ splitCaseClauses varAlt ciaohead restOfAlts
        where funbody = \(_, _, expr) -> translateFunBody expr
              funhead :: CoreAlt -> CiaoHead
              funhead = \(altcon, conargs, _) ->
                               let posArg = findInArgList (CiaoId $ hsIDtoCiaoVarID . show $ varAlt) args in
                               case posArg of
                                 Nothing -> ciaohead
                                 (Just pos) -> 
                                     case altcon of
                                       (LitAlt lit) -> replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTermLit $ translateLit lit
                                       (DataAlt datacons) -> replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTerm (CiaoId $ (hsIDtoCiaoFunctorID . show) datacons) $ map (\x -> CiaoArgTerm $ CiaoTerm (CiaoId $ (hsIDtoCiaoVarID . show) x) []) conargs
                                       DEFAULT -> ciaohead
splitCaseClauses _ _ _ = [] -- Placeholder for type-checker, there shouldn't be
                          -- any heads other than functor + args

fromCaseGetVarIDAndAlts :: Expr b -> (CiaoId, [Alt b])
fromCaseGetVarIDAndAlts (Case (Var varID) _ _ altlist) = (CiaoId $ show varID, altlist)
fromCaseGetVarIDAndAlts _ = (CiaoId "BAD_ID", []) -- If this happens, there's something wrong
                         
ciaoOnlyIdsArgList :: [String] -> [CiaoArg]
ciaoOnlyIdsArgList list = map (CiaoArgId . CiaoId) $ map (hsIDtoCiaoVarID) list

hsIDtoCiaoFunctorID :: String -> String
hsIDtoCiaoFunctorID "." = "compose"
hsIDtoCiaoFunctorID ":" = "."
hsIDtoCiaoFunctorID "True" = "true"
hsIDtoCiaoFunctorID "False" = "false"
hsIDtoCiaoFunctorID [] = []
hsIDtoCiaoFunctorID str = map (\x -> if x == '\'' then '_' else x) str
                          
hsIDtoCiaoVarID :: String -> String
hsIDtoCiaoVarID "." = "compose"
hsIDtoCiaoVarID ":" = "."
hsIDtoCiaoVarID "True" = "true"
hsIDtoCiaoVarID "False" = "false"
hsIDtoCiaoVarID [] = []
hsIDtoCiaoVarID str = let (hd:nonApostropheStr) = map (\x -> if x == '\'' then '_' else x) str in
                      (toUpper hd):nonApostropheStr
                          
funArityOfArguments :: Type -> [Int]
funArityOfArguments generalType = map typeArity $ fst $ splitFunTys generalType

typeArity :: Type -> Int 
typeArity (ForAllTy _ t) = typeArity t
typeArity (FunTy tx t) | isPredTy tx = typeArity t 
typeArity (FunTy _ t)    = 1 + typeArity t 
typeArity _              = 1 

translateFunBody :: CoreExpr -> CiaoFunctionBody
translateFunBody (Var x) = CiaoFBTerm (CiaoId ((hsIDtoCiaoVarID . show) x)) []
translateFunBody (App x (Type _)) = translateFunBody x
translateFunBody (App (Var _) (Lit lit)) = CiaoFBLit $ translateLit lit
translateFunBody (Case (App (Var varID) (Var argID)) _ _ altlist) = CiaoCaseFunCall (CiaoFBCall $ CiaoFunctionCall (CiaoId $ (hsIDtoCiaoVarID . show) varID) [CiaoFBTerm (CiaoId $ (hsIDtoCiaoVarID . show) argID) []]) $ map getTermFromCaseAlt altlist
translateFunBody (Case (App (Var varID) x) _ _ altlist) = CiaoCaseFunCall (CiaoFBCall $ CiaoFunctionCall (CiaoId $ (hsIDtoCiaoVarID . show) varID) [translateFunBody x]) $ map getTermFromCaseAlt altlist
translateFunBody (Case (Var varID) _ _ altlist) = CiaoCaseVar (CiaoId $ (hsIDtoCiaoVarID . show) varID) $ map getTermFromCaseAlt altlist
translateFunBody expr = let (functor, isfunctor) = getFunctorFromAppTree expr in
                        case isfunctor of
                          True -> CiaoFBTerm functor $ trace (show $ reverse $ collectArgsTree expr []) (reverse $ collectArgsTree expr [])
                          False -> CiaoFBCall $ CiaoFunctionCall functor $ trace (show $ reverse $ collectArgsTree expr []) (reverse $ collectArgsTree expr [])

getTermFromCaseAlt :: CoreAlt -> (CiaoFunctionBody, CiaoFunctionBody)
getTermFromCaseAlt (altcon, conargs, altExpr) = case altcon of
                                                 DataAlt datacons -> (CiaoFBTerm (CiaoId $ (hsIDtoCiaoFunctorID . show) datacons) $ map (\x -> CiaoFBTerm (CiaoId $ (hsIDtoCiaoVarID . show) x) []) conargs, translateFunBody altExpr)
                                                 LitAlt lit -> (CiaoFBLit $ translateLit lit, translateFunBody altExpr)
                                                 DEFAULT -> (CiaoEmptyFB, CiaoEmptyFB) -- placeholder

getFunctorFromAppTree :: CoreExpr -> (CiaoFunctor, Bool)
getFunctorFromAppTree (Var x) = (CiaoId $ (hsIDtoCiaoFunctorID . show) x, isDataConWorkId x)
getFunctorFromAppTree (App x _) = getFunctorFromAppTree x
getFunctorFromAppTree _ = (CiaoId "ERROR", False)

collectArgsTree :: CoreExpr -> [CiaoFunctionBody] -> [CiaoFunctionBody]
collectArgsTree (Var x) args = (CiaoFBTerm (CiaoId ((hsIDtoCiaoVarID . show) x)) []):args
collectArgsTree (App (Var _) (Var y)) args = (CiaoFBTerm (CiaoId ((hsIDtoCiaoVarID . show) y)) []):args
collectArgsTree (App x (Var y)) args = (CiaoFBTerm (CiaoId ((hsIDtoCiaoVarID . show) y)) []):(collectArgsTree x args)
collectArgsTree (App (Var _) (Type  _)) args = args
collectArgsTree (App x (Type  _)) args = collectArgsTree x args
collectArgsTree (App (Var _) app) args = (translateFunBody app):args
collectArgsTree (App x app) args = (translateFunBody app):(collectArgsTree x args)
collectArgsTree (Type _) args = args
collectArgsTree expr _ = error $ "Tree of nested App in function body has something other than Var and App: " ++ (show expr)
                           
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
