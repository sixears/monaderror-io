{- | Utilities for working with `Control.Monad.Except.MonadError` -}
module MonadError
  ( MonadError
  , ѥ, ж, ѭ, ӂ
  , eFromMaybe, eToMaybe, fromMaybe, fromRight, mapMError, mapMError'
  , modifyError, __monadError__, mErrFail, splitMError, throwError
  , leftFail, leftFailShow, leftFailP
  )
where

import Base0
import Prelude ( error )

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail, fail )

-- mtl ---------------------------------

import Control.Monad.Except  ( runExceptT )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.String   ( 𝕊 )

-------------------------------------------------------------------------------

{- | Map the exception part of a MonadError; can also be used on MonadThrow
     to convert to a MonadError (or indeed any Either). -}
mapMError ∷ ∀ ε α β μ . (MonadError ε μ, HasCallStack) ⇒ (α → ε) → 𝔼 α β → μ β
mapMError f = fromRight ∘ first f

----------------------------------------

{- | Map a monad error that is embedded within or joined by another Monad -}

mapMError' ∷ ∀ ε ι α μ . (MonadError ε μ, HasCallStack) ⇒
             (ι → ε) → ExceptT ι μ α → μ α
mapMError' f = join ∘ (mapMError f ⩺ splitMError)

----------------------------------------

{- | `fromJust`, throwing an error on Nothing -}
eFromMaybe ∷ ∀ ε α μ . (MonadError ε μ, HasCallStack) ⇒ ε → 𝕄 α → μ α
eFromMaybe e = maybe (throwError e) return

{-# DEPRECATED fromMaybe
               "use `eFromMaybe` to avoid clash with `Data.Maybe.fromMaybe`" #-}
fromMaybe ∷ ∀ ε α μ . (MonadError ε μ, HasCallStack) ⇒ ε → 𝕄 α → μ α
fromMaybe = eFromMaybe

----------------------------------------

-- | fromRight, throwing an error on Left
fromRight ∷ ∀ ε α μ . (MonadError ε μ, HasCallStack) ⇒ 𝔼 ε α → μ α
fromRight = either throwError return

----------------------------------------

{- | Split a `MonadError` out from a monad; that is, takes μ ... (which is a
     monad with an ExceptT constraint) and turns it into a layered μ (η ...) -}
splitMError ∷ ∀ ε α η μ .
              (MonadError ε η, Monad μ, HasCallStack) ⇒ ExceptT ε μ α → μ (η α)
splitMError f = either throwError return ⊳ runExceptT f

{- | Unicode alias for `splitMError` -}
ѥ ∷ ∀ ε α η μ . (MonadError ε η, Monad μ, HasCallStack) ⇒
    ExceptT ε μ α → μ (η α)
ѥ = splitMError

----------------------------------------

{- | Turn an exception into an `error`. -}
__monadError__ ∷ ∀ ε α η . (Monad η, Printable ε, HasCallStack) ⇒
                 ExceptT ε η α → η α
__monadError__ = fmap (either (error ∘ toString) id) ⊳ splitMError

{- | Unicode alias for `__monadError__` -}
ж ∷ ∀ ε α η . (Monad η, Printable ε, HasCallStack) ⇒ ExceptT ε η α → η α
ж = __monadError__

----------------------------------------

{- | Convert an either to a maybe on the RHS. -}
eToMaybe ∷ ∀ χ α . 𝔼 χ α → 𝕄 α
eToMaybe (𝕷  _) = 𝕹
eToMaybe (𝕽 a) = 𝕵 a

-- | Pronounced 'maybe-funnel', or maybe 'yus', this is an alias for `eToMaybe`.
ѭ ∷ ∀ χ α . 𝔼 χ α → 𝕄 α
ѭ = eToMaybe

----------------------------------------

{- | Convert a MonadError (or, indeed, any `Either`) to a MonadFail. -}
mErrFail ∷ ∀ ε α η . (MonadFail η, Printable ε, HasCallStack) ⇒ 𝔼 ε α → η α
mErrFail = either (fail ∘ toString) return

ӂ ∷ ∀ ε α η . (MonadFail η, Printable ε, HasCallStack) ⇒ 𝔼 ε α → η α
ӂ = mErrFail

---------------------------------------

{- | Modify the error in a MonadError. -}
modifyError ∷ ∀ ε' ε α η . MonadError ε' η ⇒ (ε → ε') → ExceptT ε η α → η α
modifyError f go = ѥ go ≫ either (throwError ∘ f) return

----------------------------------------

{-| turn an `𝔼 𝕊` into a `MonadFail` -}
leftFail ∷ (MonadFail η) ⇒ 𝔼 𝕊 α → η α
leftFail = either fail return

----------------------------------------

{-| turn an `𝔼 ε` into a `MonadFail` by `show`ing the `ε` -}
leftFailShow ∷ (MonadFail η, Show ε) ⇒ 𝔼 ε α → η α
leftFailShow = leftFail ∘ first show

----------------------------------------

{-| turn an `𝔼 ε` into a `MonadFail` by `toString`ing the `ε` -}
leftFailP ∷ (MonadFail η, Printable ε) ⇒ 𝔼 ε α → η α
leftFailP = leftFail ∘ first toString

-- that's all, folks! ---------------------------------------------------------
