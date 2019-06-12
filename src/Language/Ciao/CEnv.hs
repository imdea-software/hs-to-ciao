module Language.Ciao.CEnv where 

import Language.Ciao.Types
import Language.Ghc.Misc ()

import TysWiredIn
import GhcPlugins
import TyCoRep

import           Data.Char (toUpper)
import qualified Data.HashMap.Strict as M

data CEnv = CEnv {cIds :: M.HashMap Var (Either CFId CVId)} deriving (Show)

initCEnv :: CEnv
initCEnv = CEnv mempty

(++=) :: Var -> CEnv -> CEnv 
x ++= (CEnv γ) = CEnv $ M.insert x (Left $ toFId x) γ

(+=) :: Var -> CEnv -> CEnv 
x += (CEnv γ) = CEnv $ M.insert x (Right $ VId (toVName x) (CInfo x)) γ

(?=) :: Var -> CEnv -> [CClause]
x ?= _ 
  | x == dataConWorkId nilDataCon
  = [CBasic CListEmp]
  | x == dataConWorkId consDataCon
  = [CBasic $ CListCons []]
  | show x == "<="
  = [CBasic $ CLessEq []]
x ?= γ = case M.lookup x (cIds γ) of 
            Nothing          -> [] -- trace ("Not Found " ++ show x) [CBasic $ CVar defvar] 
            Just (Left fid)  -> [CBasic $ CFunctor fid []]
            Just (Right vid) -> [CBasic $ CVar vid]

-------------------------------------------------------------------------------
-- | Name Generation ----------------------------------------------------------
-------------------------------------------------------------------------------


toFId :: Var -> CFId 
toFId x = FId (toFName x) (typeArity (varType x)) (CInfo x)
  where 
    typeArity (ForAllTy _ t) = typeArity t
    typeArity (FunTy tx t) | isPredTy tx = typeArity t 
    typeArity (FunTy _ t)    = 1 + typeArity t 
    typeArity _              = 1 

toVId :: Var -> CVId 
toVId x = VId (toVName x) (CInfo x)

defvar :: CVId             
defvar = VId "CIAODEF" NoInfo

toVName :: Var -> String 
toVName x = map toUpper $ (showSDocUnsafe (ppr x) ++ showSDocUnsafe (ppr (varUnique x)))

toFName :: Var -> String 
toFName x = showSDocUnsafe (ppr x) -- ++ showSDocUnsafe (ppr (varName x))
