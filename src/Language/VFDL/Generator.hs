module Language.VFDL.Generator
    ( generateBlueprintJson
    ) where

import Language.VFDL.TLR
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TL

generateBlueprintJson :: [Entity] -> TL.Text
generateBlueprintJson ir = do
    encodeToLazyText ir
