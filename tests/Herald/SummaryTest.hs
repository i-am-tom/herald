{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.SummaryTest where

import Data.Aeson (Array, Object)
import Data.Int (Int32)
import Data.Monoid (Any (Any))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Herald.Schema
import Herald.Summary (Summary, summarise)
import Herald.Summary qualified as Herald
import Prelude hiding (maximum, minimum)
import Test.Hspec (Spec, describe, it, shouldBe)

spec_summaries ∷ Spec
spec_summaries = do
  describe "summaries" do
    it "meta" do
      let summaries ∷ [ Summary Array ]
          summaries = summarise do
            value
              <* title "title"
              <* deprecated
              <* description "description"
              <* example []
              <* forbid [["hello"]]

      summaries `shouldBe`
        [ mempty
            { Herald.deprecated = pure True
            , Herald.description = pure "description"
            , Herald.example = pure []
            , Herald.forbidden = [["hello"]]
            , Herald.title = pure "title"
            }
        ]

    it "array" do
      let summaries ∷ [ Summary Array ]
          summaries = summarise do
            items (string value)
              <* maxItems 10
              <* minItems 5
              <* uniqueItems

      summaries `shouldBe`
        [ mempty
            { Herald.rules =
                mempty
                  { Herald.items = Any True
                  , Herald.maxItems = pure 10
                  , Herald.minItems = pure 5
                  , Herald.uniqueItems = pure True
                  }
            }
        ]

    it "boolean" do
      let summaries ∷ [ Summary Bool ]
          summaries = summarise value

      summaries `shouldBe` [ mempty ]

    it "number" do
      let summaries ∷ [ Summary Scientific ]
          summaries = summarise do
            value
              <* maximum 10
              <* exclusiveMaximum
              <* minimum 5
              <* exclusiveMinimum
              <* multipleOf 2
              <* integer @Int32
              <* int32

      summaries `shouldBe`
        [ mempty
            { Herald.rules =
                mempty
                  { Herald.type_ = pure "integer"
                  , Herald.formatting = pure "int32"
                  , Herald.maximum = pure 10
                  , Herald.exclusiveMaximum = pure True
                  , Herald.minimum = pure 5
                  , Herald.exclusiveMinimum = pure True
                  , Herald.multipleOf = pure 2
                  }
            }
        ]

    it "object" do
      let summaries ∷ [ Summary Object ]
          summaries = summarise do
            _ ← required "foo" $ string value
            _ ← optional "bar" $ string value
            _ ← additionalProperties (Just (string value))

            readOnly
            writeOnly

            maxProperties 10
            minProperties 5

            pure ()

      summaries `shouldBe`
        [ mempty
            { Herald.rules =
                mempty
                  { Herald.elements =
                      [ ( "foo", Herald.IsRequired )
                      , ( "bar", Herald.IsOptional )
                      ]

                  , Herald.readOnly = pure True
                  , Herald.writeOnly = pure True
                  , Herald.maxProperties = pure 10
                  , Herald.minProperties = pure 5
                  , Herald.additionalProperties = pure ()
                  }
            }
        ]

    it "string" do
      let summaries ∷ [ Summary Text ]
          summaries = summarise do
            value
              <* maxLength 10
              <* minLength 5
              <* format (Custom "is-tom")
              <* pattern "^tom$"
      
      summaries `shouldBe`
        [ mempty
            { Herald.rules =
                mempty
                  { Herald.maxLength = pure 10
                  , Herald.minLength = pure 5
                  , Herald.formatted = pure (Custom "is-tom")
                  , Herald.pattern = pure "^tom$"
                  }
            }
        ]
