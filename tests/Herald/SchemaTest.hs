{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UnicodeSyntax #-}

module Herald.SchemaTest where

import Test.Hspec (SpecWith, example, it)

spec_compilation ∷ SpecWith ()
spec_compilation = do
  it "builds the model schema" do
    example $ pure ()
