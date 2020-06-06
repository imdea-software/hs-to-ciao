module Translation.DataTypesTranslation where

import CiaoSyn
import DataCon
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
