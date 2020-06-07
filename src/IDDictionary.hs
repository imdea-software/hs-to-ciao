{-# LANGUAGE OverloadedStrings #-}

module IDDictionary where

idDictionary :: String -> String
idDictionary str = case str of
  "Data.Foldable.length" -> "length"
  "Data.Foldable.foldl" -> "foldl"
  "GHC.List.filter" -> "filter"
  "(<)" -> "less"
  "(>=)" -> "great_or_eq"
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
  "GHC.Num.+" -> "plus"
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
  "GHC.Classes.>=" -> "great_or_eq"
  "GHC.Classes.==" -> "equals"
  _ -> str
