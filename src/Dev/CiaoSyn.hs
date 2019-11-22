module CiaoSyn where

import Data.List (intercalate)
    
data CiaoPred = CPredC CiaoPredC | CPredF CiaoPredF
instance Show CiaoPred where
    show (CPredC pred) = show pred
    show (CPredF pred) = show pred
              
newtype CiaoPredC = CiaoPredC [CiaoClause]
instance Show CiaoPredC where
    show (CiaoPredC []) = ""
    show (CiaoPredC clauseList) = intercalate "\n" $ map show clauseList

newtype CiaoPredF = CiaoPredF [CiaoFunction]
instance Show CiaoPredF where
    show (CiaoPredF []) = ""
    show (CiaoPredF funList) = intercalate "\n" $ map show funList
                                 
data CiaoFunction = CiaoFunction CiaoHead CiaoFunctionBody
instance Show CiaoFunction where
    show (CiaoFunction head fcall) = show head ++ " := " ++ show fcall ++ "."
    
data CiaoFunctionBody = CiaoFBTerm CiaoFunctor [CiaoFunctionBody] | CiaoFBCall CiaoFunctionCall
instance Show CiaoFunctionBody where
    show (CiaoFBTerm name arglist) =
        case arglist of
          [] -> show name
          _ -> show name ++ "(" ++ (intercalate ", " $ map show arglist) ++ ")"
    show (CiaoFBCall funcall) = show funcall

data CiaoFunctionCall = CiaoFunctionCall CiaoFunctor [CiaoFunctionBody]
instance Show CiaoFunctionCall where
    show (CiaoFunctionCall name arglist) = "~" ++ (show name) ++ "(" ++ (intercalate ", " $ map show arglist) ++ ")"

data CiaoClause = CiaoClause CiaoHead CiaoBody
instance Show CiaoClause where
    show (CiaoClause head []) = show head ++ "."
    show (CiaoClause head body) = show head ++ " :- " ++ show body ++ "."

type CiaoHead = CiaoTerm
type CiaoBody = [CiaoTerm]

-- NOTE: No support (yet) for infix variations of operators; they
-- will be used as standard prefix functors
data CiaoTerm = CiaoTerm CiaoFunctor [CiaoArg] | CiaoNumber Int | CiaoCase CiaoId [(CiaoTerm, CiaoTerm)]
instance Show CiaoTerm where
    show (CiaoTerm functor arglist) =
        let functorname = show functor in case functorname of
        -- Translates the list cons (:) to Ciao's list cons
        ":" -> ".(" ++ (intercalate "," $ map show arglist) ++ ")" 
        _ -> case arglist of
               [] -> functorname
               _ -> functorname ++ "(" ++ (intercalate ", " $ map show arglist) ++ ")"
    show (CiaoNumber x) = show x
    show (CiaoCase _ []) = "" -- dummy show, you shouldn't have an empty case
    show (CiaoCase id altlist) = "(" ++ (intercalate "\n| " $ zipWith (++) (map (((show id ++ "=") ++) . (++ " ? ")) (map (show . fst) altlist)) (map (show . snd) altlist)) ++ ")"

type CiaoFunctor = CiaoId
newtype CiaoId = CiaoId String
instance Show CiaoId where
    show (CiaoId str) = str

data CiaoArg = CiaoArgId CiaoId | CiaoArgTerm CiaoTerm
instance Show CiaoArg where
    show (CiaoArgId ciaoid) = show ciaoid
    show (CiaoArgTerm ciaoterm) = show ciaoterm
