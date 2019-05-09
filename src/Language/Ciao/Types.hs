module Language.Ciao.Types where 

import Data.List (intercalate)


-------------------------------------------------------------------------------
-- | Program Definition -------------------------------------------------------
-------------------------------------------------------------------------------

data Program i = Pg [Clause i]
data Clause i  
  = CBasic (CTerm i)            -- basic term, e.g., `append(X,[],X).`
  | CCond (CTerm i) [CTerm i]   -- term conditioned on a list of terms, e.g., t :- t1, t2, ..., tn.

data CTerm i 
  = CFunctor (FId i) [CTerm i]  -- functors
  | CVar (VId i)                -- variables
  | CLit Const                  -- Strings and Numbers 
  | CListEmp                    -- empty list
  | CListCons [CTerm i]         -- cons list: INVARIANT takes two arguments
  | CLessEq   [CTerm i]         -- term comparison: INVARIANT takes two arguments 
                                --                  TODO: extend to more comparisons
  | CEq (CTerm i) (CTerm i)     -- term equality  : INVARIANT takes two arguments
  | CNot (CTerm i)              -- term negation
  | CUnif (VId i) (CTerm i)     -- term unification: this is an intermediate term generated from case expressions

data FId i = FId {fname :: String, fArity :: Int, fInfo :: i}
data VId i = VId {vname :: String, vInfo :: i}
data Const = CInt Int | CFloat Float | CString String


-------------------------------------------------------------------------------
-- | Printing -----------------------------------------------------------------
-------------------------------------------------------------------------------

instance Show (Program i) where 
  show (Pg cs) = unlines $ map show cs 

instance Show (Clause i) where 
  show (CBasic s)   = show s ++ "."
  show (CCond s ss) = show s ++ " :- " ++ intercalate ", " (show <$> ss) ++ "."

instance Show (CTerm i) where 
  show (CFunctor f xs)   = show f ++ pars (intercalate ", " (show <$> xs))
  show (CVar x)          = show x
  show (CLit l)          = show l
  show CListEmp          = "[]"
  show (CListCons [h,t]) = "[" ++ show h ++ "|" ++ show t ++ "]"
  show (CListCons xs)    = "[" ++ pars (intercalate ", " (show <$> xs)) ++ "]"
  show (CLessEq [h,t])   = show h ++ " =< " ++ show t
  show (CLessEq xs)      = "=<" ++ pars (intercalate ", " (show <$> xs))
  show (CEq t1 t2)       = show t1 ++ " =~= " ++ show t2
  show (CUnif t1 t2)     = show t1 ++ " =~= " ++ show t2
  show (CNot t)          = "\\+" ++ pars (show t)

instance Show (FId i) where 
  show fid = fname fid 
   
instance Show (VId i) where 
  show vid = vname vid

instance Show Const where 
  show (CInt i)    = show i 
  show (CFloat f)  = show f 
  show (CString s) = show s  


pars :: String -> String 
pars x = "(" ++ x ++ ")"

-------------------------------------------------------------------------------
-- | Examples -----------------------------------------------------------------
-------------------------------------------------------------------------------

-- Example: append 
_ciaoAppend :: Program ()
_ciaoAppend = Pg [base,ind]
  where
    -- append([],L,L).  
    base = CBasic $ CFunctor cappend [CListEmp, lvar, lvar]
    -- append([H|T],L2,[H|L3])  :-  append(T,L2,L3).
    ind  = CCond ind1 [ind2] 
    ind1 = CFunctor cappend [cons lvarh lvart, lvar2, cons lvarh lvar3]
    ind2 = CFunctor cappend [lvart, lvar2, lvar3]

    cappend  = FId "append" 3 ()
    cons h t = CListCons [h, t]
    lvar     = stringToVar "L"
    lvar2    = stringToVar "L1"
    lvar3    = stringToVar "L2"
    lvarh    = stringToVar "H"
    lvart    = stringToVar "T"
    stringToVar x = CVar $ VId x ()

