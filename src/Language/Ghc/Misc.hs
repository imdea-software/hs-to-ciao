{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Ghc.Misc where 

import Outputable
import CoreSyn 
import PprCore ()
import Data.Hashable
import Unique
import Var
import Literal


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
