module Dev.Translation where

import Data.Char (toUpper)
import Debug.Trace
    
import Language.Ghc.Misc ()
import Dev.CiaoSyn
    
import CoreSyn
import GhcPlugins

-- Take into special consideration Haskell functions that return Bool

translate :: [CoreBind] -> CiaoProgram
translate binds = CiaoProgram $ map translateBind binds
                  
translateBind :: CoreBind -> CiaoPred
translateBind bind =
    case bind of
      Rec _ -> EmptyPred
      -- Rec [(var, exprBind)] -> EmptyPred -- Using EmptyPred as a placeholder
      NonRec var exprBind ->
          let name = getOccString (trace (showSDocUnsafe . ppr $ var) $ trace (showSDocUnsafe . ppr . varType $ var) var) in
          if name == "$trModule" then EmptyPred else
          clauseReturnSimpleVal (CiaoTerm (CiaoId name) $ ciaoOnlyIdsArgList arglist) fbody
              where arglist = map getOccString $ trace (showSDocUnsafe . ppr $ (fst . unfoldLam) exprBind) ((fst . unfoldLam) exprBind)
                    fbody = CiaoFBTerm (CiaoId "[]") []
      --_ -> EmptyPred

ciaoOnlyIdsArgList :: [String] -> [CiaoArg]
ciaoOnlyIdsArgList list = map (CiaoArgId . CiaoId) $ map (map toUpper) list

funArityOfArguments :: Type -> [Int]
funArityOfArguments = undefined

clauseReturnSimpleVal :: CiaoHead -> CiaoFunctionBody -> CiaoPred
clauseReturnSimpleVal ciaohead fbody = CPredF $ CiaoPredF [CiaoFunction ciaohead fbody]
                             
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go []
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)
