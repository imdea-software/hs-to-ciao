{-# LANGUAGE OverloadedStrings #-}

module HsToCiaoPP (plugin) where

--import Language.Ciao.CoreToCiao
--import Language.Ghc.Misc

--import Dev.Environment

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Dev.CiaoSyn
import Dev.DataTypesTranslation
import Dev.Embedder
import Dev.Translation
import GhcPlugins
import PrettyPrinters.AnalysisKinds
import PrettyPrinters.GeneralPrinter
import System.Directory
import System.Process
--import System.Environment (getExecutablePath)
import Text.Regex (mkRegex, subRegex)

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo =
  return (CoreDoPluginPass "CiaoTranslation" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass modguts = do
  hscEnv <- getHscEnv
  let targets = hsc_targets hscEnv
  let name = showSDocUnsafe $ pprModule $ mg_module modguts
  let fileName = map toLower name
  let definedTypes = mg_tcs modguts -- pass this to a translation
  let hsBinds = mg_binds modguts
  let ciaoCode@(CiaoProgram ciaoFunctorList) = translate targets hsBinds
  let translatedTypes = translateTypes definedTypes
  -- liftIO $ putStrLn $ "\n--- TESTS FOR TARGETS ---\n"
  -- liftIO $ putStrLn $ intercalate "\n\n" $ (map getTargetName targets)
  -- Print initial Haskell Binds
  -- liftIO $ putStrLn "\n--- Haskell Binds: ---\n"
  -- liftIO $ putStrLn $ show hsBinds
  -- Write Core bindings into the .core file
  hstociaoDir <- liftIO $ getCurrentDirectory
  liftIO $ createDirectoryIfMissing False $ hstociaoDir ++ "/out"
  liftIO $ putStrLn $ "\nHaskell Core binds have been written into the " ++ (coreFileName hstociaoDir fileName) ++ " file.\n"
  liftIO $ writeFile (coreFileName hstociaoDir fileName) (show hsBinds)
  -- Print Ciao translation
  -- liftIO $ putStrLn "\n--- Ciao Code: ---\n"
  -- liftIO $ putStrLn $ show ciaoCode
  -- Write translation into the .pl file
  liftIO $ putStrLn $ "Ciao translation of the Haskell functions has been written into the " ++ (ciaoFileName hstociaoDir fileName) ++ " file.\n"
  liftIO $ writeFile (ciaoFileName hstociaoDir fileName) ciaoModuleHeader

  liftIO $ appendFile (ciaoFileName hstociaoDir fileName) $ intercalate "\n\n" $ map show translatedTypes
  liftIO $ appendFile (ciaoFileName hstociaoDir fileName) ((singletonListSimplify . show) ciaoCode)
  ciaoPreludeContents <- liftIO $ readFile $ hstociaoDir ++ "/lib/ciao_prelude.pl"
  listOfNeededPredicates <- liftIO $ sequence $ map (tryToGetNeededPredicates ciaoPreludeContents) ciaoFunctorList
  liftIO $ appendFile (ciaoFileName hstociaoDir fileName) $ '\n' : (intercalate "\n\n" $ concat $ listOfNeededPredicates)
  maybeBoolPred <- liftIO $ errSomePredNotFound $ searchInCiaoPrelude (getCiaoPreludePredicates ciaoPreludeContents) "bool"
  liftIO $ appendFile (ciaoFileName hstociaoDir fileName) $ (\x -> case x of Nothing -> ""; (Just boolPred) -> '\n' : '\n' : boolPred) maybeBoolPred
  liftIO $ putStrLn $ "\n----------------------------------"
  liftIO $ putStrLn $ "\nExecuting Big-O analysis script:\n"
  _ <- liftIO $ rawSystem (hstociaoDir ++ "/analysis_scripts/analyze_bigo") [ciaoFileName hstociaoDir fileName]
  liftIO $ prettyPrintAnalysis fileName BigO
  bindsOnlyPass (mapM return) modguts

-- Returns the predicates it could find, and prints an error for the ones
-- it couldn't find
tryToGetNeededPredicates :: String -> CiaoFunctor -> IO [String]
tryToGetNeededPredicates ciaoPreludeContents ciaoFunctor =
  fmap catMaybes $ sequence $ map (errSubfunctorNotFound ciaoFunctor) neededPredicates
  where
    neededPredicates = fromFunctorGetNeededPredicates ciaoPreludeContents ciaoFunctor

errSubfunctorNotFound :: CiaoFunctor -> Either String String -> IO (Maybe String)
errSubfunctorNotFound functor subfunctor =
  case subfunctor of
    (Left missingFunctor) ->
      ( putStrLn $
          "WARNING: The predicate \""
            ++ missingFunctor
            ++ "\", which is required by "
            ++ (show (functorName functor) ++ "/" ++ show (functorArity functor + 1))
            ++ " wasn't found in the ciao_prelude; it's possible that the performed analysis won't work because of this.\n"
      )
        >> return Nothing
    (Right foundFunctor) -> return (Just foundFunctor)

errSomePredNotFound :: Either String String -> IO (Maybe String)
errSomePredNotFound subfunctor =
  case subfunctor of
    (Left missingFunctor) ->
      ( putStrLn $
          "WARNING: The predicate \""
            ++ missingFunctor
            ++ "\", which could be required by"
            ++ " some functor in the translation,"
            ++ " wasn't found in the ciao_prelude; it's possible that the performed analysis won't work because of this.\n"
      )
        >> return Nothing
    (Right foundFunctor) -> return (Just foundFunctor)

-- things required for the Ciao programs to work as expected
-- (module dependencies  and such), or whatever that goes before
-- the actual code, really
ciaoModuleHeader :: String
ciaoModuleHeader = ":- module(_,_,[assertions, regtypes, functional, hiord]).\n\n" -- ++
  --":- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').\n\n"
  -- CHANGE THE use_module TO USE A RELATIVE PATH INSTEAD OF AN ABSOLUTE ONE

coreFileName :: FilePath -> String -> String
coreFileName hstociaoDir fileName = hstociaoDir ++ "/out/" ++ fileName ++ ".core"

ciaoFileName :: FilePath -> String -> String
ciaoFileName hstociaoDir fileName = hstociaoDir ++ "/out/" ++ fileName ++ ".pl"

-- This function is just to make the resulting Ciao code prettier;
-- instead of leaving singleton lists in the form [X | []], it turns
-- them into something like [X], far more readable.
singletonListSimplify :: String -> String
singletonListSimplify ciaoProgram = subRegex (mkRegex "\\[(.*) \\| \\[\\]\\]") ciaoProgram "[\\1]"
