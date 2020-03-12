module Dev.DataTypesTranslation where

--import Debug.Trace

import Language.Ghc.Misc ()
import Dev.CiaoSyn
--import Dev.Translation
    
--import CoreSyn
--import TyCoRep (Type(..))
import TyCon
import DataCon
import GhcPlugins

translateTypes :: [TyCon] -> [CiaoRegtype]
translateTypes = map translateType
                 
translateType :: TyCon -> CiaoRegtype
translateType tycon = CiaoRegtype ((CiaoId $ (showSDocUnsafe . pprNameUnqualified . tyConName) tycon), map translateConstructor $ tyConDataCons tycon)

translateConstructor :: DataCon -> (CiaoId, [CiaoId])
translateConstructor con = (\(_, _, tyArgs, _) ->
                            (CiaoId $ (showSDocUnsafe . ppr . dataConName) con,
                            map (CiaoId . showSDocUnsafe . pprType) tyArgs)) $ dataConSig con
