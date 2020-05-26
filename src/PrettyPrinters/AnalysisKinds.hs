module PrettyPrinters.AnalysisKinds where
    
-- This leaves room for extensibility for future kinds of analysis    
data KindOfAnalysis = BigO

analysisFileSuffix :: KindOfAnalysis -> String
analysisFileSuffix analysisKind =
    case analysisKind of
      BigO -> "_big-o.txt"

analysisOldFileSuffix :: KindOfAnalysis -> String
analysisOldFileSuffix analysisKind =
    case analysisKind of
      BigO -> "_shfr_eterms_steps_o_co.pl"
