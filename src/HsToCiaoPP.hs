module HsToCiaoPP (plugin) where

import GhcPlugins
import Language.Ciao.Types
import Language.Ciao.CoreToCiao
import Data.Char (toLower)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install cmd todo = do
  -- reinitializeGlobals
  liftIO $ putStrLn (show cmd) 
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass modguts= do 
    let name = showSDocUnsafe $ pprModule $ mg_module modguts
    let ciaoCore = translate $ mg_binds modguts
    liftIO $ putStrLn ("Translation = \n" ++ show ciaoCore)
    liftIO $ writeFile ("./out/" ++ map toLower name ++ ".pl") (show ciaoCore)
    bindsOnlyPass (mapM (return)) modguts
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bndr@(NonRec b _) = do
          putMsgS $ ("Non-recursive binding named " ++ showSDocUnsafe (ppr b))
          return bndr 
        printBind bndr@(Rec xes) = do 
          putMsgS $ ("Recursive binding named " ++ showSDocUnsafe (ppr xes))
          return bndr