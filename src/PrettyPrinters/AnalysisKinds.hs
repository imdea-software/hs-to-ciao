module PrettyPrinters.AnalysisKinds where

-- This leaves room for extensibility for future kinds of analysis
data KindOfAnalysis = BigO | NoAnalysis deriving (Eq)

instance Show KindOfAnalysis where
  show BigO = "Big-O"
  show NoAnalysis = ""

analysisFileSuffix :: KindOfAnalysis -> String
analysisFileSuffix analysisKind =
  case analysisKind of
    BigO -> "_big-o.txt"
    NoAnalysis -> ""

analysisOldFileSuffix :: KindOfAnalysis -> String
analysisOldFileSuffix analysisKind =
  case analysisKind of
    BigO -> "_shfr_eterms_steps_o_co.pl"
    NoAnalysis -> ""
