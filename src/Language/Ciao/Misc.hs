module Language.Ciao.Misc where 

import Debug.Trace hiding (traceShow)

traceShow :: Show a => String -> a -> a 
traceShow msg t = trace ("\n\nTRACE" ++ msg ++ " " ++ show t ) t