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

type Schema ∷ Type → Type
data Schema x where
  Nullable ∷ Schema x → Schema (Maybe x)

  Array ∷ Expr switch Array x → Schema x
  Boolean ∷ Expr switch Bool x → Schema x
  Number ∷ Expr switch Scientific x → Schema x
  Object ∷ Expr switch Object x → Schema x
  String ∷ Expr switch Text x → Schema x

nullable ∷ ∀ x. Schema x → Schema (Maybe x)
nullable = Nullable

array ∷ ∀ s x. Expr s Array x → Schema x
array = Array

boolean ∷ ∀ s x. Expr s Bool x → Schema x
boolean = Boolean

number ∷ ∀ s x. Expr s Scientific x → Schema x
number = Number

object ∷ ∀ s x. Expr s Object x → Schema x
object = Object

string ∷ ∀ s x. Expr s Text x → Schema x
string = String

type Expr ∷ Switch → Type → Type → Type
type Expr switch input = Alt (ExprF switch input)

type Switch ∷ Type
data Switch = Enumerate | Constrain

type ExprF ∷ Switch → Type → Type → Type
data ExprF switch input output where
  Command ∷ Command input output → ExprF 'Constrain input output
  Deprecated ∷ ExprF switch input ()
  Description ∷ Text → ExprF switch input ()
  Example ∷ input → ExprF switch input ()
  Allow ∷ Map input output → ExprF 'Enumerate input output
  Forbid ∷ Set input → ExprF 'Constrain input ()
  Title ∷ Text → ExprF switch input ()
  Value ∷ ExprF switch input input

allow ∷ ∀ i o. Map i o → Expr 'Enumerate i o
allow = liftAlt . Allow

forbid ∷ ∀ i. Set i → Expr 'Constrain i ()
forbid = liftAlt . Forbid

command ∷ ∀ i o. Command i o → Expr 'Constrain i o
command = liftAlt . Command

deprecated ∷ ∀ s i. Expr s i ()
deprecated = liftAlt Deprecated

description ∷ ∀ s i. Text → Expr s i ()
description = liftAlt . Description

example ∷ ∀ s i. i → Expr s i ()
example = liftAlt . Example

title ∷ ∀ s i. Text → Expr s i ()
title = liftAlt . Title

value ∷ ∀ s i. Expr s i i
value = liftAlt Value

type Command ∷ Type → Type → Type
data family Command input

data instance Command Array x where
  Items ∷ Schema x → Command Array (Vector x)
  MaxItems ∷ Natural → Command Array ()
  MinItems ∷ Natural → Command Array ()
  UniqueItems ∷ Command Array ()

items ∷ ∀ x. Schema x → Expr 'Constrain Array (Vector x)
items = command . Items

maxItems ∷ Natural → Expr 'Constrain Array ()
maxItems = command . MaxItems

minItems ∷ Natural → Expr 'Constrain Array ()
minItems = command . MinItems

uniqueItems ∷ Expr 'Constrain Array ()
uniqueItems = command UniqueItems

data instance Command Bool _

data instance Command Scientific x where
  Double ∷ Command Scientific Double
  Float ∷ Command Scientific Float
  Int32 ∷ Command Scientific Int32
  Int64 ∷ Command Scientific Int64
  Integer ∷ (Bounded i, Integral i) ⇒ Command Scientific i
  ExclusiveMaximum ∷ Command Scientific ()
  ExclusiveMinimum ∷ Command Scientific ()
  Maximum ∷ Scientific → Command Scientific ()
  Minimum ∷ Scientific → Command Scientific ()
  MultipleOf ∷ Scientific → Command Scientific ()

double ∷ Expr 'Constrain Scientific Double
double = command Double

float ∷ Expr 'Constrain Scientific Float
float = command Float

int32 ∷ Expr 'Constrain Scientific Int32
int32 = command Int32

int64 ∷ Expr 'Constrain Scientific Int64
int64 = command Int64

integer ∷ ∀ i. (Bounded i, Integral i) ⇒ Expr 'Constrain Scientific i
integer = command Integer

exclusiveMaximum ∷ Expr 'Constrain Scientific ()
exclusiveMaximum = command ExclusiveMaximum

exclusiveMinimum ∷ Expr 'Constrain Scientific ()
exclusiveMinimum = command ExclusiveMinimum

maximum ∷ Scientific → Expr 'Constrain Scientific ()
maximum = command . Maximum

minimum ∷ Scientific → Expr 'Constrain Scientific ()
minimum = command . Minimum

multipleOf ∷ Scientific → Expr 'Constrain Scientific ()
multipleOf = command . MultipleOf

data instance Command Object x where
  Required ∷ Text → Schema x → Command Object x
  Optional ∷ Text → Schema x → Command Object (Maybe x)
  ReadOnly ∷ Command Object ()
  WriteOnly ∷ Command Object ()
  MaxProperties ∷ Natural → Command Object ()
  MinProperties ∷ Natural → Command Object ()
  AdditionalProperties ∷ Maybe (Schema x) → Command Object (KeyMap x)

required ∷ ∀ x. Text → Schema x → Expr 'Constrain Object x
required key = command . Required key

optional ∷ ∀ x. Text → Schema x → Expr 'Constrain Object (Maybe x)
optional key = command . Optional key

readOnly ∷ Expr 'Constrain Object ()
readOnly = command ReadOnly

writeOnly ∷ Expr 'Constrain Object ()
writeOnly = command WriteOnly

maxProperties ∷ Natural → Expr 'Constrain Object ()
maxProperties = command . MaxProperties

minProperties ∷ Natural → Expr 'Constrain Object ()
minProperties = command . MinProperties

additionalProperties ∷ ∀ x. Maybe (Schema x) → Expr 'Constrain Object (KeyMap x)
additionalProperties = command . AdditionalProperties

data instance Command Text x where
  MaxLength ∷ Natural → Command Text ()
  MinLength ∷ Natural → Command Text ()
  Format ∷ Format → Command Text ()
  Pattern ∷ String → Command Text ()

maxLength ∷ Natural → Expr 'Constrain Text ()
maxLength = command . MaxLength

minLength ∷ Natural → Expr 'Constrain Text ()
minLength = command . MinLength

format ∷ Format → Expr 'Constrain Text ()
format = command . Format

pattern ∷ String → Expr 'Constrain Text ()
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
