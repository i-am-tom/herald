{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.OpenAPITest where

import Data.Aeson ((.=), toJSON)
import Data.Aeson qualified as JSON
import Data.Set qualified as Set
import Herald.Schema
import Herald.OpenAPI (document)
import Test.Hspec (Spec, describe, it, shouldBe)

spec_examples ∷ Spec
spec_examples = do
  describe "scalar" do
    it "simple" do
      let model = boolean do
            title "Test"

      toJSON (document model) `shouldBe`
        JSON.object
          [ "title" .= JSON.String "Test"
          , "type"  .= JSON.String "boolean"
          ]

    it "decorated" do
      let model = string do
            value
              <* deprecated
              <* forbid (Set.fromList [ "foo", "bar" ])
              <* pattern "\\w+"

      toJSON (document model) `shouldBe`
        JSON.object
          [ "deprecated" .= True
          , "not" .= JSON.object
              [ "enum" .= JSON.Array [ JSON.String "bar", JSON.String "foo" ]
              ]

          , "pattern" .= JSON.String "\\w+"
          , "type" .= JSON.String "string"
          ]

    it "enumerated" do
      let model = string do
            allow [ ("foo", ()), ("bar", ()) ]
              *> value

      toJSON (document model) `shouldBe`
        JSON.object
          [ "enum" .= JSON.Array
              [ JSON.String "bar"
              , JSON.String "foo"
              ]

          , "type" .= JSON.String "string"
          ]

  describe "Composite" do
    it "simple" do
      toJSON (document (array value)) `shouldBe`
        JSON.object
          [ "type" .= JSON.String "array"
          ]

    it "decorated" do
      let model = object do
            x ← required "foo" $ string  value
            y ← optional "bar" $ boolean value

            pure (x, y)

      toJSON (document model) `shouldBe`
        JSON.object
          [ "properties" .= JSON.object
              [ "bar" .= JSON.object
                  [ "type" .= JSON.String "boolean"
                  ]

              , "foo" .= JSON.object
                  [ "type" .= JSON.String "string"
                  ]
              ]

          , "required" .= JSON.Array [ JSON.String "foo" ]
          , "type"     .= JSON.String "object"
          ]

    it "enumerated" do
      let model ∷ Schema ()
          model = object $ allow
            [ ( [ "foo" .= JSON.String "one"
                , "bar" .= JSON.String "two"
                ]
              , ()
              )

            , ( [ "baz"  .= JSON.String "three"
                , "quux" .= JSON.String "four"
                ]
              , ()
              )
            ]

      toJSON (document model) `shouldBe`
        JSON.object
          [ "enum" .= JSON.Array
              [ JSON.object
                  [ "foo" .= JSON.String "one"
                  , "bar" .= JSON.String "two"
                  ]

              , JSON.object
                  [ "baz" .= JSON.String "three"
                  , "quux" .= JSON.String "four"
                  ]
              ]

          , "type" .= JSON.String "object"
          ]
