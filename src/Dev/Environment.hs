module Dev.Environment where

--import Control.Monad.Reader
import Control.Monad.State.Strict
import CoreSyn
import HscTypes
import Var
import Outputable
    
data Environment = Environment {
      targetModuleNames :: [Target],
      letBinds :: [CoreExpr],
      boundArgs :: [Id],
      usedIdsInBody :: [Id]
    }

type Env = State Environment
    
getTargetName :: Target -> String
getTargetName = showSDocUnsafe . pprTargetId . targetId
