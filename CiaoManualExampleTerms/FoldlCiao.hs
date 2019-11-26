module FoldlCiao where

import CiaoSyn

foldlCiao :: CiaoPred
foldlCiao = CPredF (CiaoPredF [(CiaoFunction (CiaoTerm (CiaoId "foldl") [CiaoArgId (CiaoId "F"), CiaoArgId (CiaoId "BASE"), CiaoArgTerm (CiaoTerm (CiaoId "[]") [])])
                                              (CiaoFBTerm (CiaoId "BASE") [])),
                                (CiaoFunction (CiaoTerm (CiaoId "foldl") [CiaoArgId (CiaoId "F"), CiaoArgId (CiaoId "BASE"), CiaoArgTerm (CiaoTerm (CiaoId ":") [CiaoArgId (CiaoId "X"), CiaoArgId (CiaoId "XS")])])
                                              (CiaoFBCall (CiaoFunctionCall (CiaoId "foldl") [CiaoFBTerm (CiaoId "F") [], CiaoFBCall (CiaoFunctionCall (CiaoId "F") [CiaoFBTerm (CiaoId "BASE") [], CiaoFBTerm (CiaoId "X") []]), CiaoFBTerm (CiaoId "XS") []])))
                               ])
