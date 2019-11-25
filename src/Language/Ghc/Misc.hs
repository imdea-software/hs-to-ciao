{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Ghc.Misc where 

import BasicTypes
import DataCon
import Outputable
import TyCoRep 
import CoreSyn 
import PprCore ()
import Data.Hashable
import Unique
import Var
import Literal

deriving instance Show AltCon

instance Show Type where 
    show = showSDocUnsafe . ppr
           
deriving instance Show CoreBind

instance Show DataCon where
  show = showSDocUnsafe . ppr

    
instance Hashable Var where 
  hashWithSalt _ = getKey . getUnique

deriving instance Show Literal
deriving instance Show LitNumType
deriving instance Show FunctionOrData

deriving instance Show CoreExpr
instance Show (Tickish Id) where
    show = showSDocUnsafe . ppr
instance Show Coercion where
    show = showSDocUnsafe . ppr
         
instance Show Var where
    show x = showSDocUnsafe (ppr x)
