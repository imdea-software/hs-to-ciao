{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Ciao.CoreToCiao where 

import Language.Ciao.Types
import Language.Ciao.Misc
import Language.Ghc.Misc ()
import Language.Ciao.CGMonad
import Language.Ciao.CEnv

import CoreSyn 
import TysWiredIn
import GhcPlugins




translate :: [CoreBind] -> Program CInfo
translate = Pg . snd . foldl go (initCEnv, [])
  where 
    go :: (CEnv, [CClause]) -> CoreBind -> (CEnv, [CClause])
    go (γ, acc) (NonRec x e) = (γ', acc ++ translateBinds γ' (x, e))          where γ' = x ++= γ
    go (γ, acc) (Rec xes)    = (γ', acc ++ concatMap (translateBinds γ') xes) where γ' = foldl (flip (++=)) γ (fst <$> xes)


translateBinds :: CEnv -> (Var, CoreExpr) -> [CClause]
translateBinds γ (x,e) = (applyUnifies . mapTerm makeHead) <$> r
  where 
    (r, st)      = runCG (exprToTerm γ e) 
    makeHead res = CFunctor (toFId x) (((CVar . toVId) <$> (reverse $ cgArgs st)) ++ [res])


exprToTerm :: CEnv -> CoreExpr -> CG [CClause]
exprToTerm γ (Var x)
 = return  (x ?= γ)
exprToTerm _ (Lit _)
 = return [] -- TODO 
exprToTerm γ (App f e) 
  | isTypeArg e           = exprToTerm γ f 
  | isPredTy (exprType e) = exprToTerm γ f 
exprToTerm γ (App f e) = do 
  fs <- exprToTerm γ f
  es <- exprToTerm γ e
  applyAll fs es
exprToTerm γ (Lam x e)
  | isTyVar x = exprToTerm γ e 
  | isPredTy (varType x) = exprToTerm γ e 
  | otherwise = addVar x >> exprToTerm (x += γ) e 
exprToTerm _ (Coercion _) 
 = return [] 
exprToTerm _ (Type _) 
 = return [] 
exprToTerm γ (Tick _ e)
 = exprToTerm γ e 
exprToTerm γ (Cast e _)
 = exprToTerm γ e 
exprToTerm γ (Let _ e)
  = exprToTerm γ e -- TODO 
exprToTerm γ (Case e b t alts)
  = concat <$> mapM (caseToTerm γ e b t) alts


applyAll :: [CClause] -> [CClause] -> CG [CClause]
applyAll [] _   = return []
applyAll (f:fs) es = do 
  es1 <- mapM (applyOne f) es
  es2 <- applyAll fs es 
  return ([x | Just x <- es1] ++ es2) 

applyOne :: CClause -> CClause -> CG (Maybe CClause)
applyOne cf ce = do 
    cr <- applyOneTerm cf1 ce1 
    case cr of 
        Nothing -> return Nothing
        (Just c) -> return $ Just $ addCond c (cf2 ++ ce2)
  where (cf1, cf2) = splitClause cf 
        (ce1, ce2) = splitClause ce 


applyOneTerm :: CCTerm -> CCTerm -> CG (Maybe CClause)
applyOneTerm (CFunctor f es) e 
  | length es < (fArity f - 2)  = return $ Just $ CBasic $ CFunctor f (es ++ [e])
  | length es == (fArity f - 2) = do 
     x <- freshVar
     return $ Just $ CCond x [CFunctor f (es ++ [e, x])] 

applyOneTerm (CListCons es) e 
  | length es < 1  = return $ Just $ CBasic $ CListCons (es ++ [e])
  | length es == 1 = return $ Just $ CBasic $ CListCons (es ++ [e]) 

applyOneTerm (CLessEq es) e 
  | length es < 1  = return $ Just $ CBasic $ CLessEq (es ++ [e])
  | length es == 1 = return $ Just $ CBasic $ CLessEq (es ++ [e]) 

applyOneTerm _ _ = return Nothing 

caseToTerm :: CEnv -> CoreExpr -> Var -> Type -> Alt Var -> CG [CClause]
caseToTerm γ _ _ _ (DEFAULT, xs, e) 
  = exprToTerm (foldl (flip (+=)) γ xs) e 
caseToTerm _ _ _ _ (LitAlt _, _, _)
  = error "TODO: caseToTerm LitAlt"
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

litToTerm :: Literal -> CTerm CInfo
litToTerm _ = error ("TODO: litToTerm")
