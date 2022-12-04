{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.OpenAPI where

import Control.Alternative.Free (Alt (Alt), AltF (Ap, Pure))
import Control.Lens
import Data.Aeson (Array, Object, ToJSON (toJSON))
import Data.Kind (Constraint, Type)
import Data.Map.Strict qualified as Map
import Data.OpenApi qualified as OA
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Exts (fromList)
import Herald.Schema (Expr, Schema)
import Herald.Schema qualified as Schema

document ∷ Schema x → OA.Schema
document structure = go structure mempty
  where
    go ∷ Schema x → OA.Schema → OA.Schema
    go structure' acc = case structure' of
      Schema.Nullable s → go       s acc & OA.nullable ?~ True
      Schema.Array    s → branches s acc & OA.type_    ?~ OA.OpenApiArray
      Schema.Boolean  s → branches s acc & OA.type_    ?~ OA.OpenApiBoolean
      Schema.Number   s → branches s acc & OA.type_    ?~ OA.OpenApiNumber
      Schema.Object   s → branches s acc & OA.type_    ?~ OA.OpenApiObject
      Schema.String   s → branches s acc & OA.type_    ?~ OA.OpenApiString

    branches ∷ ∀ i o. Document i ⇒ Expr i o → OA.Schema → OA.Schema
    branches = \case
      Alt [     ] → id
      Alt [choix] → unwrap choix
      Alt choices → OA.oneOf ?~ [ OA.Inline (unwrap c mempty) | c ← choices ]

    unwrap ∷ ∀ i o. Document i ⇒ AltF (Schema.ExprF i) o → OA.Schema → OA.Schema
    unwrap = \case
      Pure _ → id
      Ap x f → branches f . expr x

    rejected ∷ ∀ i. Document i ⇒ Set i → OA.Referenced OA.Schema
    rejected = OA.Inline . flip expr mempty . Schema.Allow . Map.fromSet \_ → ()

    expr ∷ ∀ i o. Document i ⇒ Schema.ExprF i o → OA.Schema → OA.Schema
    expr = \case
      Schema.Allow       values → OA.enum_       ?~ map toJSON (Map.keys values)
      Schema.Forbid      values → OA.not_        ?~ rejected values
      Schema.Deprecated         → OA.deprecated  ?~ True
      Schema.Description text   → OA.description ?~ text
      Schema.Example     value  → OA.example     ?~ toJSON value
      Schema.Title       text   → OA.title       ?~ text
      Schema.Command     inner  → typed_ inner
      Schema.Value              → id

type Document ∷ Type → Constraint
class ToJSON input ⇒ Document input where
  typed_ ∷ ∀ x. Schema.Command input x → OA.Schema → OA.Schema

instance Document Bool where
  typed_ = \case

instance Document Array where
  typed_ = \case
    Schema.MaxItems    count → OA.maxItems    ?~ fromIntegral count
    Schema.MinItems    count → OA.maxItems    ?~ fromIntegral count
    Schema.UniqueItems       → OA.uniqueItems ?~ True
    Schema.Items       items → OA.items       ?~ OA.OpenApiItemsArray [OA.Inline (document items)]

instance Document Scientific where
  typed_ = \case
    Schema.Maximum          count  → OA.maximum_         ?~ count
    Schema.Minimum          count  → OA.minimum_         ?~ count
    Schema.ExclusiveMaximum        → OA.exclusiveMaximum ?~ True
    Schema.ExclusiveMinimum        → OA.exclusiveMinimum ?~ True
    Schema.MultipleOf       factor → OA.multipleOf       ?~ factor

    Schema.Double   → OA.format ?~ "double"
    Schema.Float    → OA.format ?~ "float"
    Schema.Floating → OA.type_ ?~ OA.OpenApiNumber
    Schema.Int32    → OA.format ?~ "int32"
    Schema.Int64    → OA.format ?~ "int64"
    Schema.Integer  → OA.type_ ?~ OA.OpenApiInteger

instance Document Object where
  typed_ = \case
    Schema.ReadOnly            → OA.readOnly       ?~ True
    Schema.WriteOnly           → OA.writeOnly      ?~ True
    Schema.MaxProperties count → OA.maxProperties  ?~ fromIntegral count
    Schema.MinProperties count → OA.minProperties  ?~ fromIntegral count
    Schema.Required  key value → \x → x & OA.required <>~ [key] & OA.properties <>~ fromList [( key, OA.Inline (document value) )]
    Schema.Optional  key value → OA.properties <>~ fromList [( key, OA.Inline (document value) )]

    Schema.AdditionalProperties constraint →
      OA.additionalProperties ?~ case constraint of
        Just inner → OA.AdditionalPropertiesSchema (OA.Inline (document inner))
        Nothing    → OA.AdditionalPropertiesAllowed True

instance Document Text where
  typed_ = \case
    Schema.MaxLength count  → OA.maxLength ?~ fromIntegral count
    Schema.MinLength count  → OA.minLength ?~ fromIntegral count
    Schema.Pattern   string → OA.pattern   ?~ fromString string
    Schema.Format    format → OA.format    ?~ case format of
      Schema.Byte     → "byte"
      Schema.Binary   → "binary"
      Schema.Date     → "date"
      Schema.DateTime → "date-time"
      Schema.Password → "password"
      Schema.Custom x → x
