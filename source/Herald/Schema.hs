{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.Schema where

import Control.Alternative.Free (Alt, liftAlt)
import Data.Aeson (Array, Object)
import Data.Aeson.KeyMap (KeyMap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.String (IsString (fromString))
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

nullable ∷ ∀ f x. Schema f x → Schema f (Maybe x)
nullable = Nullable

array ∷ ∀ f s x. Expr f s Array x → Schema f x
array = Array

boolean ∷ ∀ f s x. Expr f s Bool x → Schema f x
boolean = Boolean

number ∷ ∀ f s x. Expr f s Scientific x → Schema f x
number = Number

object ∷ ∀ f s x. Expr f s Object x → Schema f x
object = Object

string ∷ ∀ f s x. Expr f s Text x → Schema f x
string = String

type Expr ∷ ((Type → Type) → (Type → Type)) → Switch → Type → Type → Type
type Expr f switch input = f (ExprF f switch input)

type Switch ∷ Type
data Switch = Enumerate | Constrain

type ExprF ∷ ((Type → Type) → (Type → Type)) → Switch → Type → Type → Type
data ExprF f switch input output where
  Command ∷ Command f input output → ExprF f 'Constrain input output
  Deprecated ∷ ExprF f switch input ()
  Description ∷ Text → ExprF f switch input ()
  Example ∷ input → ExprF f switch input ()
  Allow ∷ Map input output → ExprF f 'Enumerate input output
  Forbid ∷ Set input → ExprF f 'Constrain input ()
  Title ∷ Text → ExprF f switch input ()
  Value ∷ ExprF f switch input input

allow ∷ ∀ i o. Map i o → Expr Alt 'Enumerate i o
allow = liftAlt . Allow

forbid ∷ ∀ i. Set i → Expr Alt 'Constrain i ()
forbid = liftAlt . Forbid

command ∷ ∀ i o. Command Alt i o → Expr Alt 'Constrain i o
command = liftAlt . Command

deprecated ∷ ∀ s i. Expr Alt s i ()
deprecated = liftAlt Deprecated

description ∷ ∀ s i. Text → Expr Alt s i ()
description = liftAlt . Description

example ∷ ∀ s i. i → Expr Alt s i ()
example = liftAlt . Example

title ∷ ∀ s i. Text → Expr Alt s i ()
title = liftAlt . Title

value ∷ ∀ s i. Expr Alt s i i
value = liftAlt Value

type Command ∷ ((Type → Type) → (Type → Type)) → Type → Type → Type
data family Command f input

data instance Command f Array x where
  Items ∷ Schema f x → Command f Array (Vector x)
  MaxItems ∷ Natural → Command f Array ()
  MinItems ∷ Natural → Command f Array ()
  UniqueItems ∷ Command f Array ()

items ∷ ∀ x. Schema Alt x → Expr Alt 'Constrain Array (Vector x)
items = command . Items

maxItems ∷ Natural → Expr Alt 'Constrain Array ()
maxItems = command . MaxItems

minItems ∷ Natural → Expr Alt 'Constrain Array ()
minItems = command . MinItems

uniqueItems ∷ Expr Alt 'Constrain Array ()
uniqueItems = command UniqueItems

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

double ∷ Expr Alt 'Constrain Scientific Double
double = command Double

float ∷ Expr Alt 'Constrain Scientific Float
float = command Float

int32 ∷ Expr Alt 'Constrain Scientific Int32
int32 = command Int32

int64 ∷ Expr Alt 'Constrain Scientific Int64
int64 = command Int64

integer ∷ ∀ i. (Bounded i, Integral i) ⇒ Expr Alt 'Constrain Scientific i
integer = command Integer

exclusiveMaximum ∷ Expr Alt 'Constrain Scientific ()
exclusiveMaximum = command ExclusiveMaximum

exclusiveMinimum ∷ Expr Alt 'Constrain Scientific ()
exclusiveMinimum = command ExclusiveMinimum

maximum ∷ Scientific → Expr Alt 'Constrain Scientific ()
maximum = command . Maximum

minimum ∷ Scientific → Expr Alt 'Constrain Scientific ()
minimum = command . Minimum

multipleOf ∷ Scientific → Expr Alt 'Constrain Scientific ()
multipleOf = command . MultipleOf

data instance Command f Object x where
  Required ∷ Text → Schema f x → Command f Object x
  Optional ∷ Text → Schema f x → Command f Object (Maybe x)
  ReadOnly ∷ Command f Object ()
  WriteOnly ∷ Command f Object ()
  MaxProperties ∷ Natural → Command f Object ()
  MinProperties ∷ Natural → Command f Object ()
  AdditionalProperties ∷ Maybe (Schema f x) → Command f Object (KeyMap x)

required ∷ ∀ x. Text → Schema Alt x → Expr Alt 'Constrain Object x
required key = command . Required key

optional ∷ ∀ x. Text → Schema Alt x → Expr Alt 'Constrain Object (Maybe x)
optional key = command . Optional key

readOnly ∷ Expr Alt 'Constrain Object ()
readOnly = command ReadOnly

writeOnly ∷ Expr Alt 'Constrain Object ()
writeOnly = command WriteOnly

maxProperties ∷ Natural → Expr Alt 'Constrain Object ()
maxProperties = command . MaxProperties

minProperties ∷ Natural → Expr Alt 'Constrain Object ()
minProperties = command . MinProperties

additionalProperties ∷ ∀ x. Maybe (Schema Alt x) → Expr Alt 'Constrain Object (KeyMap x)
additionalProperties = command . AdditionalProperties

data instance Command f Text x where
  MaxLength ∷ Natural → Command f Text ()
  MinLength ∷ Natural → Command f Text ()
  Format ∷ Format → Command f Text ()
  Pattern ∷ String → Command f Text ()

maxLength ∷ Natural → Expr Alt 'Constrain Text ()
maxLength = command . MaxLength

minLength ∷ Natural → Expr Alt 'Constrain Text ()
minLength = command . MinLength

format ∷ Format → Expr Alt 'Constrain Text ()
format = command . Format

pattern ∷ String → Expr Alt 'Constrain Text ()
pattern = command . Pattern

type Format ∷ Type
data Format = Date | DateTime | Password | Byte | Binary | Custom Text

date ∷ Format
date = Date

dateTime ∷ Format
dateTime = DateTime

password ∷ Format
password = Password

byte ∷ Format
byte = Byte

binary ∷ Format
binary = Binary

instance IsString Format where
  fromString = \case
    "date"      → Date
    "date-time" → DateTime
    "password"  → Password
    "byte"      → Byte
    "binary"    → Binary
    anything    → Custom (fromString anything)
