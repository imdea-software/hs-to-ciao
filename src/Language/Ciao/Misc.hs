module Language.Ciao.Misc where 

traceShow :: Show a => String -> a -> a 
traceShow msg t = trace ("\n\nTRACE" ++ msg ++ " " ++ show t ) t