module Translation.Environment where

import Control.Monad.State.Strict
import CoreSyn
import HscTypes
import Outputable
import Var

data Environment = Environment
  { targetModuleNames :: [Target],
    letBinds :: [CoreExpr],
    boundArgs :: [Id],
    usedIdsInBody :: [Id]
  }

type Env = State Environment

getTargetName :: Target -> String
getTargetName = showSDocUnsafe . pprTargetId . targetId
