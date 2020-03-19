{-# LANGUAGE OverloadedStrings #-}

module HsToCiaoPP (plugin) where

import GhcPlugins
--import Language.Ciao.CoreToCiao
-- import Language.Ghc.Misc ()
import Data.List (intercalate)
import Dev.Translation
import Dev.DataTypesTranslation
import Data.Char (toLower)
import Text.Regex (mkRegex, subRegex)

import Control.Monad.Reader
    
plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo =
  return (CoreDoPluginPass "CiaoTranslation" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass modguts= do
    hscEnv <- getHscEnv
    let targets = hsc_targets hscEnv
    let tmNames = map (showSDocUnsafe . pprTargetId . targetId) targets
    let env = Environment { targetModuleNames = tmNames }
    let name     = showSDocUnsafe $ pprModule $ mg_module modguts
    let definedTypes = mg_tcs modguts -- pass this to a translation
    let hsBinds  = mg_binds modguts
    let ciaoCore = runReader (translate hsBinds) env
    let translatedTypes = translateTypes definedTypes
    liftIO $ putStrLn $ "\n--- TESTS FOR TARGETS ---\n"
    liftIO $ putStrLn $ intercalate "\n\n" $ tmNames
    -- Print initial Haskell Binds
    liftIO $ putStrLn $ "\n--- Haskell Binds: ---\n" 
    liftIO $ putStrLn $ show hsBinds
    -- Write Core bindings into the .core file
    liftIO $ writeFile (coreFileName name) (show hsBinds)
    -- Print Ciao translation
    liftIO $ putStrLn $ "\n--- Ciao Code: ---\n"
    liftIO $ putStrLn $ show ciaoCore
    -- Write translation into the .pl file
    liftIO $ writeFile (ciaoFileName name) ciaoModuleHeader
    liftIO $ appendFile (ciaoFileName name) $ intercalate "\n\n" $ map show translatedTypes
    liftIO $ appendFile (ciaoFileName name) ((singletonListSimplify . show) ciaoCore)
    bindsOnlyPass (mapM return) modguts

-- things required for the Ciao programs to work as expected
-- (module dependencies  and such), or whatever that goes before
-- the actual code, really
ciaoModuleHeader :: String
ciaoModuleHeader = ":- module(_,_,[assertions, regtypes, functional, hiord]).\n" ++
                   ":- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').\n\n"
                  
coreFileName :: String -> String
coreFileName name = "./out/" ++ map toLower name ++ ".core"
                  
ciaoFileName :: String -> String 
ciaoFileName name = "./out/" ++ map toLower name ++ ".pl"

-- This function is just to make the resulting Ciao code prettier;
-- instead of leaving singleton lists in the form [X | []], it turns
-- them into something like [X], far more readable.
singletonListSimplify :: String -> String
singletonListSimplify ciaoProgram = subRegex (mkRegex "\\[(.*) \\| \\[\\]\\]") ciaoProgram "[\\1]"


