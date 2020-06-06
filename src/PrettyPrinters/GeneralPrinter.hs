module PrettyPrinters.GeneralPrinter where

import PrettyPrinters.AnalysisKinds
import PrettyPrinters.BigO
import System.Directory

prettyPrintAnalysis :: String -> KindOfAnalysis -> IO ()
prettyPrintAnalysis fileName analysisKind =
  case analysisKind of
    NoAnalysis -> return () -- do nothing
    _ ->
      do
        fileContents <- readFile $ "./out/" ++ fileName ++ oldFileSuffix
        let prettifiedOutput = prettifier fileContents
        if prettifiedOutput == ""
          then putStrLn "\nWARNING: Since the analysis didn't finish properly, no pretty-printing has been done.\n"
          else do
            writeFile ("./out/" ++ fileName ++ fileSuffix) $ prettifiedOutput
            removeFile $ "./out/" ++ fileName ++ oldFileSuffix
  where
    oldFileSuffix = analysisOldFileSuffix analysisKind
    fileSuffix = analysisFileSuffix analysisKind
    prettifier = analysisPrettifier analysisKind

analysisPrettifier :: KindOfAnalysis -> (String -> String)
analysisPrettifier analysisKind =
  case analysisKind of
    BigO -> prettifierBigO
    NoAnalysis -> undefined
