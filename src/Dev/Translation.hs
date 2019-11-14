module Translation where

import Language.Ghc.Misc ()
import CoreSyn
import CiaoSyn

-- Take into special consideration Haskell functions that return Bool

translate :: [CoreBind] -> [CiaoPred]
translate binds = map translateBind binds
                  
translateBind :: CoreBind -> CiaoPred
translateBind bind =
    case bind of
      Rec [(name, exprBind)] -> undefined
      NonRec name exprBind ->
          case name of
            show name == "$trModule" -> [] -- This returns the empty predicate
            _ -> CiaoClause name args
                where args = unfoldLam exprBind

bools :: String
bools = "bool(X,T) :- (X -> T=true ; T=false)."
                             
unfoldLam :: Expr b -> ([b], Expr b)
unfoldLam  = go
  where
    go acc (Lam name lamExpr) = go (name:acc) lamExpr
    go acc expr = (reverse acc, expr)
