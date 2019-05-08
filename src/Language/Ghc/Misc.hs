{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ghc.Misc where 

import Outputable
import CoreSyn 
import GhcPlugins

instance Show CoreBind where 
  show = showSDocUnsafe . ppr 
