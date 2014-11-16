{-# LANGUAGE CPP #-}

module Web.Twitter.Types.Lens.TH
       where

import Language.Haskell.TH
import Control.Lens hiding (makeLenses)

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith $ lensRules & lensField .~ nameF
  where
#if MIN_VERSION_lens(4, 4, 0)
    nameF _ n = [TopName . mkName . nameBase $ n]
#elif MIN_VERSION_lens(4, 0, 0)
    nameF = Just
#endif
