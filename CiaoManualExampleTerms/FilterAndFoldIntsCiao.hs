module FilterAndFoldIntsCiao where

import CiaoSyn

filterAndFoldIntsCiao :: CiaoPred
filterAndFoldIntsCiao = CPredF (CiaoPredF [CiaoFunction (CiaoTerm (CiaoId "filterAndFoldInts") [CiaoArgId (CiaoId "FILT"), CiaoArgId (CiaoId "F"), CiaoArgId (CiaoId "BASE"), CiaoArgId (CiaoId "LIST")])
                                           (CiaoFBCall (CiaoFunctionCall (CiaoId "compose") [CiaoFBTerm (CiaoId "foldl") [CiaoFBTerm (CiaoId "F") [], CiaoFBTerm (CiaoId "BASE") []],
                                                                                            CiaoFBTerm (CiaoId "filter") [CiaoFBTerm (CiaoId "FILT") []],
                                                                                            CiaoFBTerm (CiaoId "LIST") []]))])
