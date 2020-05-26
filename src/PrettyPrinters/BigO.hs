module PrettyPrinters.BigO where

import Data.Char
import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)
import Text.Regex.PCRE
    
type BigOFunctionName = String
    
newtype BigOArg = BigOArg String
instance Show BigOArg where
    show (BigOArg x) = cleanupExpr x

data BigOFunction = BOFunction BigOFunctionName [BigOFunction] | BOArg BigOArg
instance Show BigOFunction where
    show (BOFunction fname fargs) = case fname of
      "exp" -> (show $ fargs !! 0) ++ "^" ++ (show $ fargs !! 1)
      _ -> "(" ++ fname ++ " " ++ (intercalate " " $ map show fargs) ++ ")"
    show (BOArg arg) = show arg
    
                       
prettifierBigO :: String -> String
prettifierBigO fileContents = intercalate "\n\n" $
                              map printBigOExpr $
                              filter (isInfixOf "steps_o") $
                              splitOn "\n\n" fileContents

cleanupExpr :: String -> String
cleanupExpr = (\(x:xs) -> (toLower x):xs) . filter (/= '_')

printBigOExpr :: String -> String
printBigOExpr singleOAnalysis = lambdaName ++ " = \\" ++
                                (intercalate " " . map cleanupExpr) lambdaArgs ++
                                " -> " ++ (removeParenthesis . cleanupExpr . show $ parseBigOExpr analysisExpr)
    where removeParenthesis str = filter (\x -> x /= '(' && x /= ')') str
          lambdaArgs = init $ splitOn "," lambdaSetOfArgs
          analysisExpr = case singleOAnalysis =~ "steps_o\\((.*)\\)" of
                           [] -> error "Couldn't find the expression for the BigO function in the CiaoPP \
                                            \ analysis output file. This isn't the user's fault."
                           [matches] -> matches !! 1
                           _ -> undefined
          (lambdaName, lambdaSetOfArgs) = case singleOAnalysis =~ "pred (.*)\\((.*)\\)" of
                          [] -> error "Couldn't find the arguments of the BigO function in the CiaoPP \
                                           \ analysis output file. This isn't the user's fault."
                          [matches] -> (matches !! 1, matches !! 2)
                          _ -> undefined
                                       

parseBigOExpr :: String -> BigOFunction
parseBigOExpr analysisExpr =
    case analysisExpr =~ "(.*?)\\((.*)\\)" of
      [] -> BOArg $ BigOArg analysisExpr
      [matches] -> BOFunction (matches !! 1) $ map parseBigOExpr $ splitOn "," (matches !! 2)
      _ -> undefined
