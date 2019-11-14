import Data.List (intercalate)
                  
newtype CiaoPred = CiaoPred [CiaoClause]
instance Show CiaoPred where
    show CiaoPred [] = ""
    show CiaoPred clauseList = intercalate "\n" $ map show clauseList

data CiaoClause = CiaoClause CiaoHead CiaoBody
instance Show CiaoClause where
    show (CiaoClause head []) = show head ++ "."
    show (CiaoClause head body) = show head ++ " :- " show body ++ "."

type CiaoHead = CiaoTerm
type CiaoBody = [CiaoTerm]

data CiaoTerm = CiaoTerm CiaoFunctor [CiaoArg] | CiaoEmptyList | CiaoNumber Int
instance Show CiaoTerm where
    show CiaoTerm functor arglist = let functorname = show functor in case functorname of
        -- Translates the list cons (:) to Ciao's list cons
        ":" -> ".(" ++ (intercalate "," $ map show arglist) ++ ")" 
        _ -> functorname ++ "(" ++ (intercalate "," $ map show arglist) ++ ")"
    show CiaoEmptyList [] = "[]"
    show CiaoNumber x = show x

type CiaoFunctor = CiaoId
type CiaoId = String

data CiaoArg = CiaoArgId CiaoId | CiaoArgTerm CiaoTerm
instance Show CiaoArg where
    show CiaoArgId ciaoid = show ciaoid
    show CiaoArgTerm ciaoterm = show ciaoterm

data CiaoList = CiaoEmptyList
instance Show CiaoList where
    show CiaoEmptyList = "[]"
