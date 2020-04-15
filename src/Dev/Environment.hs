module Dev.Environment where

--import Control.Monad.Reader
import Control.Monad.State.Lazy
import CoreSyn
import HscTypes
import Var
import Outputable
    
data Environment = Environment {
      targetModuleNames :: [Target],
      letBinds :: [CoreExpr],
      boundIds :: [Id]
    }

type Env = State Environment

getTargetName :: Target -> String
getTargetName = showSDocUnsafe . pprTargetId . targetId
