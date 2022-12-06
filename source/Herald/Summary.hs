{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.Summary where

import GHC.Generics (Generic)
import Data.Aeson.Lens ()
import Control.Applicative.Free.Extra (branches, foldAp)
import Control.Lens ((%~), (.~), ASetter, at, makeLenses)
import Data.Generics.Product.Fields (field')
import Data.Aeson (Array, Object)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Kind (Constraint, Type)
import Data.Scientific (Scientific)
import Data.Map qualified as Map
import Data.Monoid (Any (Any), First (First))
import Data.Monoid.Generic (GenericMonoid (GenericMonoid))
import Data.Semigroup.Generic (GenericSemigroup (GenericSemigroup))
import Data.Set (Set)
import Data.Text (Text)
import Herald.Schema (Command (..), ExprF (..))
import Herald.Schema qualified as Herald
import Numeric.Natural (Natural)
import Prelude hiding (maximum, minimum)

type Summary ∷ Type → Type
data Summary input
  = Summary
      { allowed ∷ First (Set input)
      , deprecated ∷ First Bool
      , description ∷ First Text
      , example ∷ First input
      , forbidden ∷ Set input
      , rules ∷ Typed input
      , title ∷ First Text
      }
  deriving stock Generic

deriving stock instance (Eq input, Eq (Typed input)) ⇒ Eq (Summary input)
deriving stock instance (Ord input, Ord (Typed input)) ⇒ Ord (Summary input)
deriving stock instance (Show input, Show (Typed input)) ⇒ Show (Summary input)

deriving via (GenericSemigroup (Summary input))
  instance Summarise input ⇒ Semigroup (Summary input)

deriving via (GenericMonoid (Summary input))
  instance Summarise input ⇒ Monoid (Summary input)

type Summarise ∷ Type → Constraint
class (Monoid (Typed input), Ord input)
    ⇒ Summarise input where
  data family Typed input ∷ Type

  summarise_
    ∷ ∀ x
    . Herald.Command input x
    → Typed input
    → Typed input

makeLenses ''Summary

summarise ∷ ∀ i x. Summarise i ⇒ Herald.Expr i x → [ Summary i ]
summarise = map (foldAp (flip go) mempty) . branches
  where
    go ∷ Herald.ExprF i e → Summary i → Summary i
    go = \case
      Allow xs → field' @"allowed" ?~ Map.keysSet xs
      Command c → field' @"rules" %~ summarise_ c
      Deprecated → field' @"deprecated" ?~ True
      Description text → field' @"description" ?~ text
      Example e → field' @"example" ?~ e
      Forbid xs → field' @"forbidden" .~ xs
      Title text → field' @"title" ?~ text
      Value → id

instance Summarise Array where
  data instance Typed Array
    = Array_
        { items ∷ Any
        , maxItems ∷ First Natural
        , minItems ∷ First Natural
        , uniqueItems ∷ First Bool
        }
    deriving stock (Eq, Generic, Ord, Show)
    deriving Semigroup via (GenericSemigroup (Typed Array))
    deriving Monoid via (GenericMonoid (Typed Array))

  summarise_ = \case
    Items _ → field' @"items" .~ Any True
    MaxItems n → field' @"maxItems" ?~ n
    MinItems n → field' @"minItems" ?~ n
    UniqueItems → field' @"uniqueItems" ?~ True

instance Summarise Bool where
  newtype instance Typed Bool = Bool_ ()
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Semigroup, Monoid)

  summarise_ = \case

instance Summarise Scientific where
  data instance Typed Scientific
    = Number_
        { type_ ∷ First Text
        , formatting ∷ First Text
        , exclusiveMaximum ∷ First Bool
        , maximum ∷ First Scientific
        , exclusiveMinimum ∷ First Bool
        , minimum ∷ First Scientific
        , multipleOf ∷ First Scientific
        }
    deriving stock (Eq, Generic, Ord, Show)
    deriving Semigroup via (GenericSemigroup (Typed Scientific))
    deriving Monoid via (GenericMonoid (Typed Scientific))

  summarise_ = \case
    Floating → field' @"type_" ?~ "number"
    Double → field' @"formatting" ?~ "double"
    Float → field' @"formatting" ?~ "float"
    Integer → field' @"type_" ?~ "integer"
    Int32 → field' @"formatting" ?~ "int32"
    Int64 → field' @"formatting" ?~ "int64"
    ExclusiveMaximum → field' @"exclusiveMaximum" ?~ True
    ExclusiveMinimum → field' @"exclusiveMinimum" ?~ True
    Maximum n → field' @"maximum" ?~ n
    Minimum n → field' @"minimum" ?~ n
    MultipleOf n → field' @"multipleOf" ?~ n

type Property ∷ Type
data Property = IsOptional | IsRequired
  deriving stock (Eq, Ord, Show)

instance Summarise Object where
  data instance Typed Object
    = Object_
        { elements ∷ KeyMap Property
        , readOnly ∷ First Bool
        , writeOnly ∷ First Bool
        , maxProperties ∷ First Natural
        , minProperties ∷ First Natural
        , additionalProperties ∷ First ()
        }
    deriving stock (Eq, Generic, Ord, Show)
    deriving Semigroup via (GenericSemigroup (Typed Object))
    deriving Monoid via (GenericMonoid (Typed Object))

  summarise_ = \case
    Required k _ → field' @"elements" . at (Key.fromText k) .~ Just IsRequired
    Optional k _ → field' @"elements" . at (Key.fromText k) .~ Just IsOptional
    ReadOnly → field' @"readOnly" ?~ True
    WriteOnly → field' @"writeOnly" ?~ True
    MaxProperties n → field' @"maxProperties" ?~ n
    MinProperties n → field' @"minProperties" ?~ n
    AdditionalProperties _ → field' @"additionalProperties" ?~ ()

instance Summarise Text where
  data instance Typed Text
    = String_
        { maxLength ∷ First Natural
        , minLength ∷ First Natural
        , formatted ∷ First Herald.Format
        , pattern   ∷ First String
        }
    deriving stock (Eq, Generic, Ord, Show)
    deriving Semigroup via (GenericSemigroup (Typed Text))
    deriving Monoid via (GenericMonoid (Typed Text))

  summarise_ = \case
    Herald.MaxLength n → field' @"maxLength" ?~ n
    Herald.MinLength n → field' @"minLength" ?~ n
    Herald.Format f → field' @"formatted" ?~ f
    Herald.Pattern p → field' @"pattern" ?~ p

(?~) ∷ ASetter s t a (First b) → b → s → t
l ?~ b = l .~ First (Just b)

infixr 4 ?~
