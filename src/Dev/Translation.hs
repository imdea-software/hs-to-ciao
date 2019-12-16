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
      Rec _ -> placeholderPred
      -- Rec [(var, exprBind)] -> EmptyPred -- Using EmptyPred as a placeholder
      NonRec var exprBind ->
          -- let name = show (trace (showSDocUnsafe . ppr $ var) $
          --                  trace (showSDocUnsafe . ppr . varType $ var) $
          --                  trace (show . funArityOfArguments . varType $ var) var) in
          if name == "$trModule" then placeholderPred else
          (metapred, clauseReturnSimpleVal (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) fbody)
              where arglist = map show $ trace (show $ (fst . unfoldLam) exprBind) ((fst . unfoldLam) exprBind)
                    fbody = CiaoFBTerm (CiaoId "[]") []
                    metapred = CiaoMetaPred (name, funArityOfArguments $ varType var)
                    name = hsIDtoCiaoFunctorID $
                           show (trace (showSDocUnsafe . ppr $ var) $
                                 trace (showSDocUnsafe . ppr . varType $ var) $
                                 trace (show . funArityOfArguments . varType $ var) var)
      --_ -> EmptyPred

-- :- meta_predicate foldl(pred(3),?,?,?).

                            
ciaoOnlyIdsArgList :: [String] -> [CiaoArg]
ciaoOnlyIdsArgList list = map (CiaoArgId . CiaoId) $ map (hsIDtoCiaoVarID) list

hsIDtoCiaoFunctorID :: String -> String
hsIDtoCiaoFunctorID str = map (\x -> if x == '\'' then '_' else x) str
                          
hsIDtoCiaoVarID :: String -> String
hsIDtoCiaoVarID str = map (\x -> if x == '\'' then '_' else toUpper x) str
                          
funArityOfArguments :: Type -> [Int]
funArityOfArguments generalType = map typeArity $ fst $ splitFunTys generalType

typeArity :: Type -> Int 
typeArity (ForAllTy _ t) = typeArity t
typeArity (FunTy tx t) | isPredTy tx = typeArity t 
typeArity (FunTy _ t)    = 1 + typeArity t 
typeArity _              = 1 

clauseReturnSimpleVal :: CiaoHead -> CiaoFunctionBody -> CiaoPred
clauseReturnSimpleVal ciaohead fbody = CPredF $ CiaoPredF [CiaoFunction ciaohead fbody]

splitCaseClauses :: CiaoId -> CiaoHead -> [CoreAlt] -> [CiaoFunction]
splitCaseClauses varAlt ciaohead@(CiaoTerm _ args) altlist =
    [ CiaoFunction (funhead alt) (funbody alt) | alt <- altlist ] 
        where funbody = \(_, _, expr) -> translateFunBody expr
              funhead :: CoreAlt -> CiaoHead
              funhead = \(altcon, conargs, _) ->
                               let posArg = findInArgList varAlt args in
                               case posArg of
                                 Nothing -> ciaohead
                                 (Just pos) -> 
                                     case altcon of
                                       (LitAlt lit) -> replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTermLit $ translateLit lit
                                       (DataAlt datacons) -> replaceArgInHeadWith ciaohead pos $ CiaoArgTerm $ CiaoTerm (CiaoId $ show datacons) $ map (\x -> CiaoArgTerm $ CiaoTerm (CiaoId $ show x) []) conargs
                                       DEFAULT -> ciaohead
splitCaseClauses _ _ _ = [] -- Placeholder for type-checker, there shouldn't be
                          -- any heads other than functor + args

replaceArgInHeadWith :: CiaoHead -> Int -> CiaoArg -> CiaoHead
replaceArgInHeadWith (CiaoTerm functor arglist) poshead arghead = CiaoTerm functor $ replace arglist poshead arghead
    where replace [] _ _ = []
          replace (_:xs) 0 arg = arg:xs
          replace (_:xs) pos arg = replace xs (pos - 1) arg
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
                                          
                       
translateFunBody :: CoreExpr -> CiaoFunctionBody
translateFunBody = undefined

-- For now this just translates the Literal as its immediate
-- String representation; will probably have to change in the future
translateLit :: Literal -> CiaoLiteral
translateLit lit = CiaoLitStr $ (showSDocUnsafe . ppr) lit
    
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)
