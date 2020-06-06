module Translation.DataTypesTranslation where

import DataCon
import CiaoSyn
import GhcPlugins
import Translation.CoreInstances ()

translateTypes :: [TyCon] -> [CiaoRegtype]
translateTypes = map translateType

translateType :: TyCon -> CiaoRegtype
translateType tycon = CiaoRegtype ((CiaoId $ (showSDocUnsafe . pprNameUnqualified . tyConName) tycon), map translateConstructor $ tyConDataCons tycon)

translateConstructor :: DataCon -> (CiaoId, [CiaoId])
translateConstructor con =
  ( \(_, _, tyArgs, _) ->
      ( CiaoId $ (showSDocUnsafe . ppr . dataConName) con,
        map (CiaoId . showSDocUnsafe . pprType) tyArgs
      )
  )
    $ dataConSig con
