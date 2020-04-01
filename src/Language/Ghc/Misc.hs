{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Ghc.Misc where 

import Prelude hiding ((<>))
    
import BasicTypes
import DataCon
import Outputable
import TyCoRep 
import CoreSyn 
import PprCore ()
import Data.Hashable
import Unique
import DynFlags
import Var 
import Literal
import Dev.Environment
import Name (pprPrefixName)-- (pprOccName, getOccName)
deriving instance Show AltCon

instance Show Type where 
    show = showSDocUnsafe . ppr
           
deriving instance Show CoreBind
-- instance Show CoreBind where 
--   show = showSDocUnsafe . ppr 

instance Show DataCon where
  show = showSDocUnsafe . ppr

showHsID :: Outputable a => Environment -> a -> String
-- showHsID _ = showSDocForUser unsafeGlobalDynFlags alwaysQualify . ppr
showHsID env x = let name = (showSDocForUser unsafeGlobalDynFlags alwaysQualify . ppr) x
                     prefix = takeWhile (/= '.') name in
                 if elem prefix (map ((takeWhile (/= '.')) . tail . (dropWhile (/= '/')) . getTargetName) $ targetModuleNames env) then
                     tail . (dropWhile (/= '.')) $ name
                 else name
                  
    
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
    show x = showSDocUnsafe (pprPrefixName x) -- pprOccName $ getOccName
