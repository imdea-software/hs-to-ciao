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
                             
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)
