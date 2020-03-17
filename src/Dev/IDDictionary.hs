{-# LANGUAGE OverloadedStrings #-}

module Dev.IDDictionary where
    
--import Data.Text (replace)

--substringIDDictionary :: String -> String
                         
idDictionary :: String -> String
idDictionary str = case str of                     
                     "(<)" -> "less"
                     "(-)" -> "substract"
                     "." -> "compose"
                     ":" -> "."
                     "GHC.Base.$" -> "fun_apply"
                     "GHC.Types.[]" -> "[]"
                     "GHC.Types.True" -> "true"
                     "GHC.Types.False" -> "false"
                     "GHC.Num.*" -> "mult"
                     "GHC.Num.-" -> "substract"
                     "GHC.Num.+" -> "sum"
                     "(:)" -> "."
                     "GHC.Types.:" -> "."
                     "True" -> "true"
                     "False" -> "false"
                     "++" -> "append"
                     "GHC.Base.++" -> "append"
                     "GHC.Base.map" -> "map"
                     "GHC.Enum.enumFromTo" -> "enumfromto"
                     "GHC.Classes.<" -> "less"
                     _ -> str
