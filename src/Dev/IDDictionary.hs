{-# LANGUAGE OverloadedStrings #-}

module Dev.IDDictionary where
    
--import Data.Text (replace)

--substringIDDictionary :: String -> String
                         
idDictionary :: String -> String
idDictionary "(<)" = "less"
idDictionary "(-)" = "substract"
idDictionary "." = "compose"
idDictionary ":" = "."
idDictionary "GHC.Types.[]" = "[]"
idDictionary "GHC.Types.True" = "true"
idDictionary "GHC.Types.False" = "false"
idDictionary "GHC.Num.*" = "mult"
idDictionary "GHC.Num.-" = "substract"
idDictionary "(:)" = "."
idDictionary "GHC.Types.:" = "."
idDictionary "True" = "true"
idDictionary "False" = "false"
idDictionary "++" = "append"
idDictionary "GHC.Base.++" = "append"
idDictionary "GHC.Base.map" = "map"
idDictionary "GHC.Enum.enumFromTo" = "enumfromto"
idDictionary "GHC.Classes.<" = "less"
idDictionary x = x
