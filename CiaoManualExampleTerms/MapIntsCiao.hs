module MapIntsCiao where

import CiaoSyn

mapIntsCiao :: CiaoPred
mapIntsCiao = CPredF (CiaoPredF [CiaoFunction (CiaoTerm (CiaoId "mapInts") [CiaoArgId (CiaoId "DS_D11N"), CiaoArgTerm (CiaoTerm (CiaoId "[]") [])])
                                              (CiaoFBTerm (CiaoId "[]") []),
                                 CiaoFunction (CiaoTerm (CiaoId "mapInts") [CiaoArgId (CiaoId "DS_D11N"), CiaoArgTerm (CiaoTerm (CiaoId ".") [CiaoArgId (CiaoId "X"), CiaoArgId (CiaoId "XS")])])
                                              (CiaoFBTerm (CiaoId ".") [CiaoFBCall (CiaoFunctionCall (CiaoId "DS_D11N") [CiaoFBTerm (CiaoId "X") []]), CiaoFBCall (CiaoFunctionCall (CiaoId "mapInts") [CiaoFBTerm (CiaoId "DS_D11N") [], CiaoFBTerm (CiaoId "XS") []])])
                                ])
