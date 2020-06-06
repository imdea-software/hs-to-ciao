{-# LANGUAGE OverloadedStrings #-}

module HsToCiaoPP (plugin) where

import CiaoSyn
import Control.Monad.Loops (iterateWhile)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
import Embedder
import GhcPlugins
import PrettyPrinters.AnalysisKinds
import PrettyPrinters.GeneralPrinter
import System.Directory
import System.Process
import Text.Regex (mkRegex, subRegex)
import Translation.DataTypesTranslation
import Translation.MainTranslation

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
  liftIO $ putStrLn strWelcomeUser -- This string is defined down in this module
  analysisOption <- liftIO $ iterateWhile (== Nothing) analysisSelect
  let analysisKind = fromJust analysisOption -- analysisOption shouldn't be Nothing, given the previous line
  liftIO $ putStrLn "\n----------------------------------"
  hscEnv <- getHscEnv
  let targets = hsc_targets hscEnv
  let name = showSDocUnsafe $ pprModule $ mg_module modguts
  let fileName = map toLower name
  let definedTypes = mg_tcs modguts
  let hsBinds = mg_binds modguts
  let ciaoCode@(CiaoProgram ciaoFunctorList) = translate targets hsBinds
  let translatedTypes = translateTypes definedTypes
  hstociaoDir <- liftIO $ getCurrentDirectory
  liftIO $ createDirectoryIfMissing False $ hstociaoDir ++ "/out"
  liftIO $ putStrLn $ "\nHaskell Core binds have been written into the " ++ (coreFileName hstociaoDir fileName) ++ " file.\n"
  liftIO $ writeFile (coreFileName hstociaoDir fileName) (show hsBinds)
  liftIO $ putStrLn $ "Ciao translation of the Haskell functions has been written into the " ++ (ciaoFileName hstociaoDir fileName) ++ " file."
  liftIO $ writeFile (ciaoFileName hstociaoDir fileName) $ ciaoModuleHeader hstociaoDir
  liftIO $ appendFile (ciaoFileName hstociaoDir fileName) $ intercalate "\n\n" $ map show translatedTypes
  liftIO $ appendFile (ciaoFileName hstociaoDir fileName) ((singletonListSimplify . show) ciaoCode)
  case analysisKind of
    NoAnalysis -> return () -- do nothing
    _ -> do
      ciaoPreludeContents <- liftIO $ readFile $ hstociaoDir ++ "/lib/ciao_prelude.pl"
      listOfNeededPredicates <- liftIO $ sequence $ map (tryToGetNeededPredicates ciaoPreludeContents) ciaoFunctorList
      liftIO $ appendFile (ciaoFileName hstociaoDir fileName) $ '\n' : (intercalate "\n\n" $ concat $ listOfNeededPredicates)
      maybeBoolPred <- liftIO $ errSomePredNotFound $ searchInCiaoPrelude (getCiaoPreludePredicates ciaoPreludeContents) "bool"
      liftIO $ appendFile (ciaoFileName hstociaoDir fileName) $ (\x -> case x of Nothing -> ""; (Just boolPred) -> '\n' : '\n' : boolPred) maybeBoolPred
      liftIO $ putStrLn $ "\n----------------------------------"
      liftIO $ putStrLn $ "\nExecuting " ++ (show analysisKind) ++ " analysis script:\n"
      _ <- liftIO $ rawSystem (hstociaoDir ++ (locateAnalysisScript analysisKind)) [ciaoFileName hstociaoDir fileName]
      liftIO $ prettyPrintAnalysis fileName analysisKind
  bindsOnlyPass (mapM return) modguts

-- This is the function you'd want to extend if you were to add
-- more kinds of analysis to the plugin, just add more options
analysisSelect :: IO (Maybe KindOfAnalysis)
analysisSelect =
  do
    optionSelected <- getLine
    case optionSelected of
      "1" -> return $ Just BigO
      "2" -> return $ Just NoAnalysis
      _ -> do
        putStrLn "The selected option is invalid. Please, select a valid option."
        return Nothing

locateAnalysisScript :: KindOfAnalysis -> String
locateAnalysisScript analysisKind =
  case analysisKind of
    BigO -> "/analysis_scripts/analyze_bigo"
    NoAnalysis -> ""

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
ciaoModuleHeader :: FilePath -> String
ciaoModuleHeader _ = ":- module(_,_,[assertions, regtypes, functional, hiord]).\n\n" -- ++
--":- use_module('" ++ hstociaoDir ++ "/lib/ciao_prelude.pl').\n\n"

coreFileName :: FilePath -> String -> String
coreFileName hstociaoDir fileName = hstociaoDir ++ "/out/" ++ fileName ++ ".core"

ciaoFileName :: FilePath -> String -> String
ciaoFileName hstociaoDir fileName = hstociaoDir ++ "/out/" ++ fileName ++ ".pl"

-- This function is just to make the resulting Ciao code prettier;
-- instead of leaving singleton lists in the form [X | []], it turns
-- them into something like [X], far more readable.
singletonListSimplify :: String -> String
singletonListSimplify ciaoProgram = subRegex (mkRegex "\\[(.*) \\| \\[\\]\\]") ciaoProgram "[\\1]"

strWelcomeUser :: String
strWelcomeUser =
  "\nBefore performing an analysis to your specified file(s),\nplease choose "
    ++ "the kind of analysis you would like to have performed:\n\n"
    ++ "1) Big-O analysis\n"
    ++ "2) No analysis (just translation)\n\n"
    ++ "Type in the number corresponding to the analysis kind and press ENTER:"
