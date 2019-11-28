{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Ciao.CGMonad where 

import Language.Ciao.Types
import Language.Ghc.Misc ()

import GhcPlugins

import Control.Monad.State


type CG = State CGEnv

runCG :: CG a -> (a, CGEnv)
runCG act = runState act initCGEnv 

data CGEnv = CG {cgArgs :: [Var], cgIndx :: Int}

initCGEnv :: CGEnv
initCGEnv = CG [] 0

addVar :: Var -> CG () 
addVar x = modify $ \s -> s {cgArgs = x:cgArgs s}

freshVar :: CG CCTerm
freshVar = do 
    i <- cgIndx <$> get
    modify $ \s -> s{cgIndx = i+1}
    return $ CVar (VId ("CIAO"++show i) CILocal)

