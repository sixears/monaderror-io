{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Utilities for working with `Control.Monad.Except.MonadError` -}
module MonadError
  ( MonadError, ѥ, fromMaybe, fromRight
  , mapMError, mapMError', eFromMaybe, runExceptT, splitMError, throwError )
where

import Prelude ( )

-- base --------------------------------

import Control.Monad   ( Monad, join, return )
import Data.Bifunctor  ( first )
import Data.Either     ( Either, either )
import Data.Maybe      ( Maybe, maybe )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, ExceptT, runExceptT, throwError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )

-------------------------------------------------------------------------------

{- | Map the exception part of a MonadError; can also be used on MonadThrow
     to convert to a MonadError (or indeed any Either). -}
mapMError ∷ (MonadError β μ) ⇒ (α → β) → Either α x → μ x
mapMError f = fromRight ∘ first f

----------------------------------------

{- | Map a monad error that is embedded within or joined by another Monad -}

mapMError' ∷ MonadError ε μ ⇒ (ι → ε) → ExceptT ι μ α → μ α
-- mapMError' f = join ∘ fmap (mapMError f) ∘ splitMError
mapMError' f = join ∘ (mapMError f ⩺ splitMError)

----------------------------------------

{- | `fromJust`, throwing an error on Nothing -}
eFromMaybe ∷ MonadError ε μ ⇒ ε → Maybe α → μ α
eFromMaybe e = maybe (throwError e) return

{-# DEPRECATED fromMaybe "use `eFromMaybe` to avoid clash with `Data.Maybe.fromMaybe`" #-}
fromMaybe ∷ MonadError ε μ ⇒ ε → Maybe α → μ α
fromMaybe = eFromMaybe

----------------------------------------

-- | fromRight, throwing an error on Left
fromRight ∷ MonadError ε μ ⇒ Either ε α → μ α
fromRight = either throwError return

----------------------------------------

{- | Split a `MonadError` out from a monad; that is, takes μ ... (which is a
     monad with an ExceptT constraint) and turns it into a layered μ (η ...) -}
splitMError ∷ (MonadError ε η, Monad μ) ⇒ ExceptT ε μ a → μ (η a)
splitMError f = either throwError return ⊳ runExceptT f

{- | Unicode alias for `splitMError` -}
ѥ ∷ (MonadError ε η, Monad μ) ⇒ ExceptT ε μ α → μ (η α)
ѥ = splitMError

-- that's all, folks! ---------------------------------------------------------
