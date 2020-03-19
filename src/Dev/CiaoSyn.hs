module Dev.CiaoSyn where

import Data.List (intercalate)
import Data.Char (toLower)

newtype CiaoRegtype = CiaoRegtype (CiaoId, [(CiaoId, [CiaoId])])
instance Show CiaoRegtype where
    show (CiaoRegtype (regtypeName, listOfCons)) =
        ":- regtype " ++ (showTypeID regtypeName) ++ "/1.\n" ++ (intercalate "\n" $ map showCons listOfCons)
            where showCons = (\(tyConsName, tyConsArgs) ->
                              let varIDs = genVarIDs (length tyConsArgs) in
                              case length tyConsArgs of
                                0 -> showTypeID regtypeName ++ "(" ++ showTypeID tyConsName ++ ")" ++ "."
                                _ -> showTypeID regtypeName ++ "(" ++ showTypeID tyConsName ++ "(" ++ intercalate ", " varIDs ++  ")) :- " ++
                                     (intercalate ", " $ zipWith (\tyCons varID -> showTypeID tyCons ++ "(" ++ varID ++ ")") tyConsArgs varIDs) ++ ".\n\n")
                  genVarIDs = (\len -> map (("X"++) . show) [1..len])

showTypeID :: CiaoId -> String
showTypeID (CiaoId "") = ""
showTypeID (CiaoId str@(x:xs)) =
    case str of
      "Int" -> "num"
      _ -> (toLower x):xs
    
newtype CiaoProgram = CiaoProgram [(CiaoMetaPred, CiaoPred)]
instance Show CiaoProgram where
    show (CiaoProgram prediclist) = intercalate "\n" $ map showAndConcatTuple prediclist
        where showAndConcatTuple = (\(x,y) -> show x ++ "\n" ++ show y)
                                   
newtype CiaoMetaPred = CiaoMetaPred (String, [Int])
instance Show CiaoMetaPred where
    show (CiaoMetaPred (_, [])) = ""
    show (CiaoMetaPred (predname, arities)) = ":- meta_predicate " ++ predname ++ "(" ++ (intercalate "," (map (\x -> if x == 1 then "?" else "pred(" ++ show x ++ ")") arities)) ++ ",?)."
                                    
data CiaoPred = CPredC CiaoPredC | CPredF CiaoPredF | EmptyPred
instance Show CiaoPred where
    show (CPredC predic) = show predic
    show (CPredF predic) = show predic
    show EmptyPred = "" -- EmptyPred is for placeholder purposes
              
newtype CiaoPredC = CiaoPredC [CiaoClause]
instance Show CiaoPredC where
    show (CiaoPredC []) = ""
    show (CiaoPredC clauseList) = (intercalate "\n" $ map show clauseList) ++ "\n"

newtype CiaoPredF = CiaoPredF [CiaoFunction]
instance Show CiaoPredF where
    show (CiaoPredF []) = ""
    show (CiaoPredF funList) = (intercalate "\n" $ map show funList) ++ "\n"
                                 
data CiaoFunction = CiaoFunction CiaoHead CiaoFunctionBody
instance Show CiaoFunction where
    show (CiaoFunction ciaohead fcall) = show ciaohead ++ " := " ++ show fcall ++ "."
    
data CiaoFunctionBody = CiaoFBTerm CiaoFunctor [CiaoFunctionBody] | CiaoFBCall CiaoFunctionCall | CiaoFBLit CiaoLiteral | CiaoCaseVar CiaoId [(CiaoFunctionBody, CiaoFunctionBody)] | CiaoCaseFunCall CiaoFunctionBody [(CiaoFunctionBody, CiaoFunctionBody)] | CiaoEmptyFB
instance Show CiaoFunctionBody where
    show (CiaoFBTerm name arglist) =
        case arglist of
          [] -> show name
          _ -> case name of
                 (CiaoId ".") -> "[" ++ (intercalate " | " $ map show arglist) ++ "]"
                 _ -> show name ++ "(" ++ (intercalate ", " $ map show arglist) ++ ")"
    show (CiaoFBCall funcall) = show funcall
    show (CiaoFBLit lit) = show lit
    show (CiaoCaseVar _ []) = "" -- dummy show, you shouldn't have an empty case
    show (CiaoCaseFunCall _ []) = "" -- dummy show, you shouldn't have an empty case
    show (CiaoCaseVar ciaoid altlist) = "(" ++ (intercalate "\n| " $ zipWith (++) (map (((show ciaoid ++ "=") ++) . (++ " ? ")) (map (show . fst) altlist)) (map (show . snd) altlist)) ++ ")"
    show (CiaoCaseFunCall ciaoid altlist) = "(" ++ (intercalate "\n| " $ zipWith (++) (map (((show ciaoid ++ "=") ++) . (++ " ? ")) (map (show . fst) altlist)) (map (show . snd) altlist)) ++ ")"
    show CiaoEmptyFB = "" -- placeholder body

data CiaoFunctionCall = CiaoFunctionCall CiaoFunctor [CiaoFunctionBody]
instance Show CiaoFunctionCall where
    show (CiaoFunctionCall name arglist) = "~" ++ (show name) ++ "(" ++ (intercalate ", " $ map show arglist) ++ ")"

data CiaoClause = CiaoClause CiaoHead CiaoBody
instance Show CiaoClause where
    show (CiaoClause ciaohead []) = show ciaohead ++ "."
    show (CiaoClause ciaohead body) = show ciaohead ++ " :- " ++ show body ++ "."

type CiaoHead = CiaoTerm
type CiaoBody = [CiaoTerm]

-- NOTE: No support (yet) for infix variations of operators; they
-- will be used as standard prefix functors
data CiaoTerm = CiaoTerm CiaoFunctor [CiaoArg] | CiaoTermLit CiaoLiteral | CiaoNumber Int | CiaoEmptyTerm
instance Show CiaoTerm where
    show (CiaoTerm functor arglist) =
        let functorname = show functor in case functorname of
        -- Translates the list cons (:) to Ciao's list cons
        --":" -> ".(" ++ (intercalate "," $ map show arglist) ++ ")" 
        _ -> case arglist of
               [] -> functorname
               _ -> case functorname of
                      "." -> "[" ++ (intercalate " | " $ map show arglist) ++ "]"
                      _ -> functorname ++ "(" ++ (intercalate ", " $ map show arglist) ++ ")"
    show (CiaoTermLit lit) = show lit
    show (CiaoNumber x) = show x
    show CiaoEmptyTerm = "" -- this should only be used with placeholders

-- add fields with record syntax and 'data'                     
type CiaoFunctor = CiaoId
newtype CiaoId = CiaoId String deriving Eq
instance Show CiaoId where
    show (CiaoId str) = str

data CiaoArg = CiaoArgId CiaoId | CiaoArgTerm CiaoTerm
instance Show CiaoArg where
    show (CiaoArgId ciaoid) = show ciaoid
    show (CiaoArgTerm ciaoterm) = show ciaoterm

data CiaoLiteral = CiaoLitStr String
instance Show CiaoLiteral where
    show (CiaoLitStr str) = str
