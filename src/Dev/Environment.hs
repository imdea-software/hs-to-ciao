module Dev.Environment where

import Control.Monad.Reader
import CoreSyn
import HscTypes
import Outputable
    
data Environment = Environment {
      targetModuleNames :: [Target],
      letBinds :: [CoreExpr]
    }

type Env = Reader Environment

getTargetName :: Target -> String
getTargetName = showSDocUnsafe . pprTargetId . targetId
