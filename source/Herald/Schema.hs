{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.Schema where

import Data.Aeson (Array, Object)
import Data.Aeson.KeyMap (KeyMap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Numeric.Natural (Natural)
import Data.Int (Int32, Int64)

type Schema ∷ ((Type → Type) → (Type → Type)) → (Type → Type)
data Schema f x where
  Nullable ∷ Schema f x → Schema f (Maybe x)

  Array ∷ Expr f switch Array x → Schema f x
  Boolean ∷ Expr f switch Bool x → Schema f x
  Number ∷ Expr f switch Scientific x → Schema f x
  Object ∷ Expr f switch Object x → Schema f x
  String ∷ Expr f switch Text x → Schema f x

type Expr ∷ ((Type → Type) → (Type → Type)) → Switch → Type → Type → Type
type Expr f switch input = f (ExprF f switch input)

type Switch ∷ Type
data Switch = Enumerate | Constrain

type ExprF ∷ ((Type → Type) → (Type → Type)) → Switch → Type → Type → Type
data ExprF f switch input output where
  Command ∷ Command f input output → ExprF f 'Constrain input output
  Deprecate ∷ ExprF f switch input ()
  Description ∷ Text → ExprF f switch input ()
  Allow ∷ Map input output → ExprF f 'Enumerate input output
  Forbid ∷ Set input → ExprF f 'Constrain input ()
  Name ∷ Text → ExprF f switch input ()
  Title ∷ Text → ExprF f switch input ()
  Value ∷ ExprF f switch input input

type Command ∷ ((Type → Type) → (Type → Type)) → Type → Type → Type
data family Command f input

data instance Command f Array x where
  Items ∷ Schema f x → Command f Array (Vector x)
  MaxItems ∷ Natural → Command f Array ()
  MinItems ∷ Natural → Command f Array ()
  UniqueItems ∷ Command f Array ()

data instance Command f Bool _

data instance Command f Scientific x where
  Double ∷ Command f Scientific Double
  Float ∷ Command f Scientific Float
  Int32 ∷ Command f Scientific Int32
  Int64 ∷ Command f Scientific Int64
  Integer ∷ (Bounded i, Integral i) ⇒ Command f Scientific i
  ExclusiveMaximum ∷ Command f Scientific ()
  ExclusiveMinimum ∷ Command f Scientific ()
  Maximum ∷ Scientific → Command f Scientific ()
  Minimum ∷ Scientific → Command f Scientific ()
  MultipleOf ∷ Scientific → Command f Scientific ()

type Inclusivity ∷ Type
data Inclusivity = Exclusive | Inclusive

data instance Command f Object x where
  Required ∷ Text → Schema f x → Command f Object x
  Optional ∷ Text → Schema f x → Command f Object (Maybe x)
  ReadOnly ∷ Command f Object ()
  WriteOnly ∷ Command f Object ()
  MaxProperties ∷ Natural → Command f Object ()
  MinProperties ∷ Natural → Command f Object ()
  AdditionalProperties ∷ Schema f x → Command f Object (KeyMap x)

data instance Command f Text x where
  MaxLength ∷ Natural → Command f Text ()
  MinLength ∷ Natural → Command f Text ()
  Format ∷ Format → Command f Text ()
  Pattern ∷ String → Command f Text ()

type Format ∷ Type
data Format = Date | DateTime | Password | Byte | Binary | Custom Text
