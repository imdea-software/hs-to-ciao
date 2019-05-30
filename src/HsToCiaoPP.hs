module HsToCiaoPP (plugin) where

import GhcPlugins
import Language.Ciao.CoreToCiao
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
    liftIO $ putStrLn $ "Haskell Binds:" 
    liftIO $ putStrLn $ show hsBinds 
    -- Print Ciao translation
    liftIO $ putStrLn $ "Ciao Code:"
    liftIO $ putStrLn $ show ciaoCore  
    -- Write tranlsation into the .pl file 
    liftIO $ writeFile (ciaoFileName name) (show ciaoCore)
    bindsOnlyPass (mapM return) modguts

ciaoFileName :: String -> String 
ciaoFileName name = "./out/" ++ map toLower name ++ ".pl"


