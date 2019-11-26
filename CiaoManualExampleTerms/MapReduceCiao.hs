module MapReduceCiao where

import CiaoSyn

mapreduceCiao :: CiaoPred
mapreduceCiao = CPredF (CiaoPredF [CiaoFunction (CiaoTerm (CiaoId "mapreduce") [CiaoArgId (CiaoId "F"), CiaoArgId (CiaoId "COMBINATOR"), CiaoArgId (CiaoId "BASE"), CiaoArgId (CiaoId "LIST")])
                                           (CiaoFBCall (CiaoFunctionCall (CiaoId "compose") [CiaoFBTerm (CiaoId "foldl") [CiaoFBTerm (CiaoId "COMBINATOR") [], CiaoFBTerm (CiaoId "BASE") []],
                                                                                            CiaoFBTerm (CiaoId "map") [CiaoFBTerm (CiaoId "F") []],
                                                                                            CiaoFBTerm (CiaoId "LIST") []]))])
