module HsToCiaoPP (plugin) where

import GhcPlugins
--import Language.Ciao.CoreToCiao
import Dev.Translation
import Data.Char (toLower)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo =
  return (CoreDoPluginPass "CiaoTranslation" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass modguts= do 
    let name     = showSDocUnsafe $ pprModule $ mg_module modguts
    let hsBinds  = mg_binds modguts
    let ciaoCore = translate hsBinds  
    -- Print initial Haskell Binds 
    liftIO $ putStrLn $ "\n--- Haskell Binds: ---\n" 
    liftIO $ putStrLn $ show hsBinds 
    -- Print Ciao translation
    liftIO $ putStrLn $ "\n--- Ciao Code: ---\n"
    liftIO $ putStrLn $ show ciaoCore
    -- Write Core bindings into the .core file
    liftIO $ writeFile (coreFileName name) (show hsBinds)
    -- Write translation into the .pl file
    liftIO $ writeFile (ciaoFileName name) ciaoModuleHeader
    liftIO $ appendFile (ciaoFileName name) (show ciaoCore)
    bindsOnlyPass (mapM return) modguts

-- things required for the Ciao programs to work as expected
-- (module dependencies  and such), or whatever that goes before
-- the actual code, really
ciaoModuleHeader :: String
ciaoModuleHeader = ":- module(_,_,[functional, hiord]).\n" ++
                   ":- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').\n\n"
                  
coreFileName :: String -> String
coreFileName name = "./out/" ++ map toLower name ++ ".core"
                  
ciaoFileName :: String -> String 
ciaoFileName name = "./out/" ++ map toLower name ++ ".pl"


