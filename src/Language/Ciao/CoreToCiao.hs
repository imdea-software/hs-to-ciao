{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ciao.CoreToCiao where 

import Language.Ciao.Types
import CoreSyn 
import TysWiredIn
import GhcPlugins
import Data.Hashable
import Unique

import TyCoRep



import Data.Char (toUpper)
import qualified Data.HashMap.Strict as M
import Control.Monad.State

import Debug.Trace hiding (traceShow)

 
translate :: [CoreBind] -> Program CInfo
translate bs = Pg $ go initCEnv bs 
  where 
    go _ [] = [] 
    go γ (NonRec x e:bs) = translateBinds (x ++= γ) (x, e) ++  go (x ++= γ) bs 
    go γ (Rec xes:bs)    = concatMap (translateBinds (foldl (flip (++=)) γ (fst <$> xes))) xes ++  go (foldl (flip (++=)) γ (fst <$> xes)) bs 


translateBinds :: CEnv -> (Var, CoreExpr) -> [Clause CInfo]
translateBinds γ (x,e) = map (mapTerm hd) r -- if null (cgTerms st) then (CBasic . hd) <$> r else (\c -> CCond (hd c) (cgTerms st)) <$> r
  where 
    (r, st) = runState (exprToTerm γ e) initCGEnv
    hd r = CFunctor (toFId x) (((CVar . toVId) <$> (reverse $ cgArgs st)) ++ [r])
    mapTerm f (CBasic t) = CBasic (f t)
    mapTerm f (CCond t ts) = applyUnifies (f t) ts
    
applyUnifies t ts = let r = go [] t ts in trace ("\n\nRESULT" ++ show r ++ "\nBEFORE\n" ++ show (t,ts)) r 
  where 
    go []  t [] = CBasic t
    go acc t [] = CCond t (reverse acc) 
    go acc t (CUnif x tx: ts) = go (subst x tx <$> acc) (subst x tx t) (subst x tx <$> ts)
    go acc t (c:ts) = go (c:acc) t ts

subst x ex (CFunctor f ts) = CFunctor f (subst x ex <$> ts)
subst x ex (CVar y)
  | vname x == vname y = ex 
  | otherwise          = CVar y 
subst x ex (CListCons es) = CListCons (subst x ex <$> es)
subst x ex (CLessEq es)   = CLessEq (subst x ex <$> es)
subst x ex (CEq t1 t2)    = CEq (subst x ex t1) (subst x ex t2)
subst x ex (CUnif y e)    = CUnif y (subst x ex e)
subst x ex (CNot t)       = CNot (subst x ex t)
subst _ _ (CLit c) = CLit c 
subst _ _ CListEmp = CListEmp



data CEnv = CEnv {cIds :: M.HashMap Var (Either (FId CInfo) (VId CInfo))}
data CInfo = CInfo Var | NoInfo | CILocal
data CGEnv = CG {cgArgs :: [Var], cgTerms :: [CTerm CInfo], cgIndx :: Int, cgUnifies :: [(Var, CTerm CInfo)]}

initCGEnv :: CGEnv
initCGEnv = CG [] [] 0 []

initCEnv :: CEnv
initCEnv = CEnv mempty

exprToTerm :: CEnv -> CoreExpr -> State CGEnv [Clause CInfo]
exprToTerm γ (Var x)
 = return  (x ?= γ)
exprToTerm γ (Lit l)
 = return $ [CBasic $ litToTerm l] 
exprToTerm γ (App f e) | isTypeArg e
  = exprToTerm γ f 
exprToTerm γ (App f e) | isPredTy (exprType e)
  = exprToTerm γ f 
exprToTerm γ ee@(App f e) = do 
  fs <- exprToTerm γ f
  es <- exprToTerm γ e
  applyAll fs es
exprToTerm γ (Lam x e)
  | isTyVar x = exprToTerm γ e 
  | isPredTy (varType x) = traceShow ("Is Pred " ++ show x ) <$> exprToTerm γ e 
  | otherwise = addVar x >> exprToTerm (x += γ) e 
exprToTerm _ (Coercion _) 
 = return [] 
exprToTerm _ (Type _) 
 = return [] 
exprToTerm γ (Tick _ e)
 = exprToTerm γ e 
exprToTerm γ (Cast e _)
 = exprToTerm γ e 
exprToTerm γ ee@(Let _ e)
  = exprToTerm γ e -- error ("Let " ++ show ee) 
exprToTerm γ (Case e b t alts)
  = concat <$> mapM (caseToTerm γ e b t) alts


applyAll :: [Clause CInfo] -> [Clause CInfo] -> State CGEnv [Clause CInfo]
applyAll [] _   = return []
applyAll (f:fs) es = do 
  es1 <- mapM (applyOne f) es
  es2 <- applyAll fs es 
  return ([x | Just x <- es1] ++ es2) 

applyOne :: Clause CInfo -> Clause CInfo -> State CGEnv (Maybe (Clause CInfo))
applyOne cf ce = do 
    cr <- applyOneTerm cf1 ce1 
    case cr of 
        Nothing -> return Nothing
        (Just c) -> return $ Just $ addCond c (cf2 ++ ce2)
  where (cf1, cf2) = splitClause cf 
        (ce1, ce2) = splitClause ce 


splitClause (CBasic t) = (t, [])
splitClause (CCond t ts) = (t,ts)

addCond t [] = t 
addCond (CBasic t) ts = CCond t ts
addCond (CCond t ts) ts' = CCond t (ts ++ ts')



applyOneTerm :: CTerm CInfo -> CTerm CInfo -> State CGEnv (Maybe (Clause CInfo))
applyOneTerm (CFunctor f es) e 
  | length es < (fArity f - 2) = return $ Just $ CBasic $ CFunctor f (es ++ [e])
  | length es == (fArity f - 2) = do 
     x <- freshVar
     return $ Just $ CCond x [CFunctor f (es ++ [e, x])] 

applyOneTerm (CListCons es) e 
  | length es < 1  = return $ Just $ CBasic $ CListCons (es ++ [e])
  | length es == 1 = return $ Just $ CBasic $ CListCons (es ++ [e]) 

applyOneTerm (CLessEq es) e 
  | length es < 1  = return $ Just $ CBasic $ CLessEq (es ++ [e])
  | length es == 1 = return $ Just $ CBasic $ CLessEq (es ++ [e]) 

applyOneTerm f e = return $ traceShow ("HERE HERE HERE " ++ show (f,e)) Nothing 

unify :: Var -> CTerm CInfo -> State CGEnv ()
unify x e = modify $ \s -> s{cgUnifies = (x,e):(cgUnifies s)} 


caseToTerm :: CEnv -> CoreExpr -> Var -> Type -> Alt Var -> State CGEnv [Clause CInfo]
caseToTerm γ _ _ _ (DEFAULT, xs, e) 
  = exprToTerm (foldl (flip (+=)) γ xs) e 
caseToTerm γ _ _ _ (LitAlt _, _, e)
  = undefined 
caseToTerm γ ee _ _ (DataAlt c, xs, e)
  | c == nilDataCon 
  = do cee <- exprToTerm γ ee 
       ce  <- exprToTerm γ e 
       return [addCond t (toCondUnify tc (CListEmp)) | t <- ce, tc <- cee]
  | c == consDataCon 
  = do cee <- exprToTerm γ ee 
       ce  <- exprToTerm (foldl (flip (+=)) γ xs) e 
       return [addCond t (toCondUnify tc (CListCons ( (CVar . toVId) <$> xs))) | t <- ce, tc <- cee]
  | c == trueDataCon 
  = do cee <- exprToTerm γ ee 
       ce  <- exprToTerm (foldl (flip (+=)) γ xs) e 
       return [addCond t (termCond tc) | t <- ce, tc <- cee]
  | c == falseDataCon 
  = do cee <- exprToTerm γ ee 
       ce  <- exprToTerm (foldl (flip (+=)) γ xs) e 
       return [addCond t (termCond $ mapTerm CNot tc) | t <- ce, tc <- cee]
  | otherwise -- TODO
  = do 
    cee <- exprToTerm γ ee
    traceShow ("CASE = " ++ show e ++ "\nCOND = " ++ show cee  ++ "\nFROM EXPR = " ++ show ee) <$> 
     exprToTerm (foldl (flip (+=)) γ xs) e 

mapTerm f (CBasic t) = CBasic $ f t 
mapTerm f (CCond t ts) = CCond (f t) ts

termCond (CBasic t) = [t]
termCond (CCond t ts) = t:ts  

toCondUnify (CBasic (CVar x)) t = [CUnif x t]
toCondUnify (CCond (CVar x) ts) t = CUnif x t:ts  
toCondUnify (CBasic t1) t = [CEq t1 t]
toCondUnify (CCond t1 ts) t = CEq t1 t:ts

litToTerm :: Literal -> CTerm CInfo
litToTerm = undefined 

(++=) :: Var -> CEnv -> CEnv 
x ++= (CEnv γ) = CEnv $ M.insert x (Left $ toFId x) γ

toFId :: Var -> FId CInfo
toFId x = trace ("arity " ++ show x ++ " == " ++ show (typeArity (varType x))) $ 
  FId (toFName x) (typeArity (varType x)) (CInfo x)
  where 
    typeArity (ForAllTy _ t) = typeArity t
    typeArity (FunTy tx t) | isPredTy tx = typeArity t 
    typeArity (FunTy _ t)    = 1 + typeArity t 
    typeArity t              = 1 

(+=) :: Var -> CEnv -> CEnv 
x += (CEnv γ) = CEnv $ M.insert x (Right $ VId (toVName x) (CInfo x)) γ

addVar :: Var -> State CGEnv () 
addVar x = modify $ \s -> s {cgArgs = x:cgArgs s}

addTerm :: CTerm CInfo -> State CGEnv ()
addTerm t = modify $ \s -> s{cgTerms = t:cgTerms s}

freshVar :: State CGEnv (CTerm CInfo)
freshVar = do 
    i <- cgIndx <$> get
    modify $ \s -> s{cgIndx = i+1}
    return $ CVar (VId ("CIAO"++show i) CILocal)


toVId :: Var -> VId CInfo
toVId x = VId (toVName x) (CInfo x)
toVName :: Var -> String 
toVName x = map toUpper $ (showSDocUnsafe (ppr x) ++ showSDocUnsafe (ppr (varUnique x)))


toFName :: Var -> String 
toFName x = showSDocUnsafe (ppr x) -- ++ showSDocUnsafe (ppr (varName x))


(?=) :: Var -> CEnv -> [Clause CInfo]
x ?= _ 
  | x == dataConWorkId nilDataCon
  = [CBasic CListEmp]
  | x == dataConWorkId consDataCon
  = [CBasic $ CListCons []]
  | show x == "<="
  = [CBasic $ CLessEq []]
x ?= γ = case M.lookup x (cIds γ) of 
            Nothing          -> trace ("Not Found " ++ show x) [CBasic $ CVar defvar] 
            Just (Left fid)  -> [CBasic $ CFunctor fid []]
            Just (Right vid) -> [CBasic $ CVar vid]

defvar = VId "CIAODEF" NoInfo

instance Show CEnv where 
  show (CEnv g) = show g 

instance Show CoreExpr where 
    show x = showSDocUnsafe (ppr x)
  
instance Show Var where 
  show x = showSDocUnsafe (ppr x)

instance Hashable Var where 
    hashWithSalt _ = getKey . getUnique

traceShow :: Show a => String -> a -> a 
traceShow msg t = trace ("\n\nTRACE" ++ msg ++ " " ++ show t ) t