{-# LANGUAGE OverloadedStrings #-}

module Dev.IDDictionary where
    
--import Data.Text (replace)

--substringIDDictionary :: String -> String
                         
idDictionary :: String -> String
idDictionary str = case str of
                     "Data.Foldable.length" -> "length"
                     "Data.Foldable.foldl" -> "foldl"
                     "GHC.List.filter" -> "filter"
                     "(<)" -> "less"
                     "(-)" -> "substract"
                     "." -> "compose"
                     ":" -> "."
                     "GHC.Base.$" -> "fun_apply"
                     "GHC.Base.." -> "compose"
                     "GHC.Types.[]" -> "[]"
                     "GHC.Types.True" -> "true"
                     "GHC.Types.False" -> "false"
                     "GHC.Num.*" -> "mult"
                     "GHC.Num.-" -> "substract"
                     "GHC.Num.+" -> "sum"
                     "GHC.Real.div" -> "div"
                     "(:)" -> "."
                     "GHC.Types.:" -> "."
                     "True" -> "true"
                     "False" -> "false"
                     "++" -> "append"
                     "GHC.Base.++" -> "append"
                     "GHC.Base.map" -> "map"
                     "GHC.Enum.enumFromTo" -> "enumfromto"
                     "GHC.Classes.<" -> "less"
                     "GHC.Classes.==" -> "equals"
                     _ -> str
