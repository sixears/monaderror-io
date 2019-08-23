{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError.IO
  ( asIOError, hoistMonadIOError, ioMonadErr, ioMonadError
  , wrapAsIOErr, wrapIOErr )
where

-- base --------------------------------

import Control.Exception.Base  ( IOException )
import Control.Monad           ( (>>=), join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Either             ( Either, either )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import System.IO               ( IO )
import System.IO.Error         ( catchIOError )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )
import Control.Lens.Review   ( (#) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError.IO.Error  ( AsIOError, IOError, _IOErr ) 
import MonadError           ( mapMError, splitMError )  

-------------------------------------------------------------------------------

{- | Perform some `IO`, catching any `IOError`s into a `MonadError`. -}
ioMonadError ∷ (MonadIO μ, MonadError IOException η) ⇒ IO α → μ (η α)
ioMonadError io = liftIO $ catchIOError (return ⊳ io) (return ∘ throwError)

----------------------------------------

{- | Hoist an IO (Either IOException α) to `MonadError`/`MonadIO`. -}
hoistMonadIOError ∷ (MonadIO μ, AsIOError ε, MonadError ε μ)
                  ⇒ IO (Either IOException α) → μ α
hoistMonadIOError eio = liftIO eio >>= mapMError (_IOErr #)

----------------------------------------

{- | Perform some `IO`, catching any `IOError`s into a `MonadError` (in a
     `MonadIO`) -}
asIOError ∷ (AsIOError ε, MonadIO μ, MonadError ε μ) ⇒ IO α → μ α
asIOError = hoistMonadIOError ∘ ioMonadError

----------------------------------------

{- | `asIOError` specialized to `IOError` -}
ioMonadErr ∷ (MonadIO μ, MonadError IOError μ) ⇒ IO α → μ α
ioMonadErr = asIOError

----------------------------------------

_wrapIOErr ∷ MonadError ε μ ⇒ (ι → ε) → (β → ExceptT ι μ α) → β → μ α
_wrapIOErr f g =
  join ∘ fmap (either throwError return) ∘ fmap (first f) ∘ splitMError ∘ g

----------------------------------------

{- | Take an IO Action, convert it to a MonadError-function, wrapping any
     `AsIOError` with a custom type -}
wrapAsIOErr ∷ (MonadIO μ, AsIOError ι, MonadError ε μ) ⇒ (ι → ε) → IO α → μ α
wrapAsIOErr f = _wrapIOErr f asIOError

----------------------------------------

{- | Take an IO Action, convert it to a MonadError-function, wrapping any
     `IOError` with a custom type.
 -}
wrapIOErr ∷ (MonadIO μ, MonadError ε μ) ⇒ (IOError → ε) → IO α → μ α
wrapIOErr f = _wrapIOErr f ioMonadErr

-- that's all, folks! ---------------------------------------------------------
