{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Applicative.Free.Extra where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Control.Alternative.Free (Alt, alternatives)
import Control.Alternative.Free qualified as Alt
import Control.Applicative.Free (Ap (Ap, Pure), runAp)
import Control.Monad.State (State, execState, modify)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Proxy (Proxy (Proxy))

branches ∷ Alt f x → [ Ap f x ]
branches xs = alternatives xs >>= \case
  Alt.Pure x → [ Pure x ]
  Alt.Ap x f → map (Ap x) (branches f)

foldAp ∷ ∀ f s x. (∀ e. s → f e → s) → s → Ap f x → s
foldAp f acc = runIdentity . foldApM (\s → Identity . f s) acc

foldApM ∷ ∀ m f s x. Monad m ⇒ (∀ e. s → f e → m s) → s → Ap f x → m s
foldApM f x = flip execState (pure x) . getCompose . runAp (cast . modify . lift)
  where
    cast ∷ ∀ u s'. State s' () → Compose (State s') Proxy u
    cast = Compose . fmap \_ → Proxy

    lift ∷ ∀ e. f e → m s → m s
    lift e s = s >>= flip f e
