{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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

type Schema ∷ Type → Type
data Schema x where
  Nullable ∷ Schema x → Schema (Maybe x)
  Array ∷ Expr Array x → Schema x
  Boolean ∷ Expr Bool x → Schema x
  Number ∷ Expr Scientific x → Schema x
  Object ∷ Expr Object x → Schema x
  String ∷ Expr Text x → Schema x

nullable ∷ ∀ x. Schema x → Schema (Maybe x)
nullable = Nullable

array ∷ ∀ x. Expr Array x → Schema x
array = Array

boolean ∷ ∀ x. Expr Bool x → Schema x
boolean = Boolean

number ∷ ∀ x. Expr Scientific x → Schema x
number = Number

object ∷ ∀ x. Expr Object x → Schema x
object = Object

string ∷ ∀ x. Expr Text x → Schema x
string = String

type Expr ∷ Type → Type → Type
type Expr input = Alt (ExprF input)

type ExprF ∷ Type → Type → Type
data ExprF input output where
  Allow ∷ Map input output → ExprF input output
  Command ∷ Command input output → ExprF input output
  Deprecated ∷ ExprF input ()
  Description ∷ Text → ExprF input ()
  Example ∷ input → ExprF input ()
  Forbid ∷ Set input → ExprF input ()
  Title ∷ Text → ExprF input ()
  Value ∷ ExprF input input

allow ∷ ∀ i o. Map i o → Expr i o
allow = liftAlt . Allow

forbid ∷ ∀ i. Set i → Expr i ()
forbid = liftAlt . Forbid

command ∷ ∀ i o. Command i o → Expr i o
command = liftAlt . Command

deprecated ∷ ∀ i. Expr i ()
deprecated = liftAlt Deprecated

description ∷ ∀ i. Text → Expr i ()
description = liftAlt . Description

example ∷ ∀ i. i → Expr i ()
example = liftAlt . Example

title ∷ ∀ i. Text → Expr i ()
title = liftAlt . Title

value ∷ ∀ i. Expr i i
value = liftAlt Value

type Command ∷ Type → Type → Type
data family Command input

data instance Command Array x where
  Items ∷ Schema x → Command Array (Vector x)
  MaxItems ∷ Natural → Command Array ()
  MinItems ∷ Natural → Command Array ()
  UniqueItems ∷ Command Array ()

items ∷ ∀ x. Schema x → Expr Array (Vector x)
items = command . Items

maxItems ∷ Natural → Expr Array ()
maxItems = command . MaxItems

minItems ∷ Natural → Expr Array ()
minItems = command . MinItems

uniqueItems ∷ Expr Array ()
uniqueItems = command UniqueItems

data instance Command Bool _

data instance Command Scientific x where
  Floating ∷ RealFloat n ⇒ Command Scientific n
  Double ∷ Command Scientific ()
  Float ∷ Command Scientific ()
  Integer ∷ (Bounded i, Integral i) ⇒ Command Scientific i
  Int32 ∷ Command Scientific ()
  Int64 ∷ Command Scientific ()
  ExclusiveMaximum ∷ Command Scientific ()
  ExclusiveMinimum ∷ Command Scientific ()
  Maximum ∷ Scientific → Command Scientific ()
  Minimum ∷ Scientific → Command Scientific ()
  MultipleOf ∷ Scientific → Command Scientific ()

floating ∷ ∀ n. RealFloat n ⇒ Expr Scientific n
floating = command Floating

double ∷ Expr Scientific ()
double = command Double

float ∷ Expr Scientific ()
float = command Float

int32 ∷ Expr Scientific ()
int32 = command Int32

int64 ∷ Expr Scientific ()
int64 = command Int64

integer ∷ ∀ i. (Bounded i, Integral i) ⇒ Expr Scientific i
integer = command Integer

exclusiveMaximum ∷ Expr Scientific ()
exclusiveMaximum = command ExclusiveMaximum

exclusiveMinimum ∷ Expr Scientific ()
exclusiveMinimum = command ExclusiveMinimum

maximum ∷ Scientific → Expr Scientific ()
maximum = command . Maximum

minimum ∷ Scientific → Expr Scientific ()
minimum = command . Minimum

multipleOf ∷ Scientific → Expr Scientific ()
multipleOf = command . MultipleOf

data instance Command Object x where
  Required ∷ Text → Schema x → Command Object x
  Optional ∷ Text → Schema x → Command Object (Maybe x)
  ReadOnly ∷ Command Object ()
  WriteOnly ∷ Command Object ()
  MaxProperties ∷ Natural → Command Object ()
  MinProperties ∷ Natural → Command Object ()
  AdditionalProperties ∷ Maybe (Schema x) → Command Object (KeyMap x)

required ∷ ∀ x. Text → Schema x → Expr Object x
required key = command . Required key

optional ∷ ∀ x. Text → Schema x → Expr Object (Maybe x)
optional key = command . Optional key

readOnly ∷ Expr Object ()
readOnly = command ReadOnly

writeOnly ∷ Expr Object ()
writeOnly = command WriteOnly

maxProperties ∷ Natural → Expr Object ()
maxProperties = command . MaxProperties

minProperties ∷ Natural → Expr Object ()
minProperties = command . MinProperties

additionalProperties ∷ ∀ x. Maybe (Schema x) → Expr Object (KeyMap x)
additionalProperties = command . AdditionalProperties

data instance Command Text x where
  MaxLength ∷ Natural → Command Text ()
  MinLength ∷ Natural → Command Text ()
  Format ∷ Format → Command Text ()
  Pattern ∷ String → Command Text ()

maxLength ∷ Natural → Expr Text ()
maxLength = command . MaxLength

minLength ∷ Natural → Expr Text ()
minLength = command . MinLength

format ∷ Format → Expr Text ()
format = command . Format

pattern ∷ String → Expr Text ()
pattern = command . Pattern

type Format ∷ Type
data Format = Date | DateTime | Password | Byte | Binary | Custom Text
  deriving stock (Eq, Ord, Show)

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
