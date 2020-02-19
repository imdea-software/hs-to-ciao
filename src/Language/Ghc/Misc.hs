{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Ghc.Misc where 

import Outputable
import TyCoRep 
import CoreSyn 
import PprCore ()
import Data.Hashable
import Unique
import Var
import Literal

import DynFlags


showQualified :: Var -> String 
showQualified = showSDocForUser unsafeGlobalDynFlags alwaysQualify . ppr


vars :: [CoreBind] -> [Var]
vars = concatMap go 
  where 
    go (NonRec x e) = x:exprVars e
    go (Rec xes)    = (fst <$> xes) ++ concatMap exprVars (snd <$> xes)

    exprVars :: CoreExpr -> [Var]
    exprVars (Var x)        = [x]
    exprVars (App e1 e2)    = exprVars e1 ++ exprVars e2 
    exprVars (Lam x e)      = x:exprVars e  
    exprVars (Let b e)      = go b ++ exprVars e 
    exprVars (Cast e _)     = exprVars e 
    exprVars (Tick _ e)     = exprVars e 
    exprVars (Case _ _ _ _) = [] -- NV TODO
    exprVars _              = [] 

instance Show AltCon where 
  show = showSDocUnsafe . ppr 


instance Show Type where 
    show = showSDocUnsafe . ppr 
instance Show CoreBind where 
  show = showSDocUnsafe . ppr 
instance Hashable Var where 
  hashWithSalt _ = getKey . getUnique


instance Show Literal where 
  show x = showSDocUnsafe (ppr x)

instance Show CoreExpr where 
  show x = showSDocUnsafe (ppr x)
  
instance Show Var where 
  show x = showSDocUnsafe (ppr x)
