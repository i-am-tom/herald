{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Orphan instances for lensing into key maps.
module Data.Aeson.KeyMap.Extended where

import Control.Lens
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap

type instance Index   (KeyMap x) = Key
type instance IxValue (KeyMap x) = x
instance      Ixed    (KeyMap x)

instance At (KeyMap x) where
  at k f = KeyMap.alterF f k
