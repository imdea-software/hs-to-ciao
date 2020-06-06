module Embedder where

import CiaoSyn
import Data.List (find)
import Data.List.Split (splitOn)
import Text.Regex (matchRegex, mkRegex)

{- WARNING: This is a quite nasty workaround. This is just to have a way
to embed the lib/ciao_prelude.pl predicates into the translated file so it
can be analyzed. This shouldn't be necessary, but CiaoPP is somewhat buggy right
now and won't accept `use_module` or `include` as a way to use auxiliary predicates.
-}

-- Right value is the correct predicate definition as found in ciao_prelude;
-- Left value is the name of the predicate that couldn't be found
fromFunctorGetNeededPredicates :: String -> CiaoFunctor -> [Either String String]
fromFunctorGetNeededPredicates ciaoPreludeFile functor = neededPredicates
  where
    internalCiaoPrelude = getCiaoPreludePredicates ciaoPreludeFile
    neededPredicates = map (searchInCiaoPrelude internalCiaoPrelude) $ functorSubfunctorIds functor

getCiaoPreludePredicates :: String -> [String]
getCiaoPreludePredicates ciaoPreludeFile = splitOn "\n\n" ciaoPreludeFile

searchInCiaoPrelude :: [String] -> String -> Either String String
searchInCiaoPrelude internalCiaoPrelude predName = case maybePred of
  (Just predDefinition) -> Right predDefinition
  Nothing -> Left predName
  where
    maybePred = find (\ciaoPreludePred -> (matchRegex searchRegex ciaoPreludePred) /= Nothing) internalCiaoPrelude
    searchRegex = mkRegex $ "(:-)?.*(" ++ predName ++ ").*((:-)|(:=))"
