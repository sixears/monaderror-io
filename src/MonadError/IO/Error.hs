{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError.IO.Error
  ( AsIOError(..), IOError(..)
  , ioError, isNoSuchThingError, isPermError
  , squashIOErrs, squashIOErrsB
  , squashNoSuchThing, squashNoSuchThingT, squashNoSuchThingB, userE
  )
where

-- base --------------------------------

import qualified  System.IO.Error  as  SysIOError

import Control.Exception       ( Exception )
import Control.Exception.Base  ( IOException )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Control.Monad           ( join, return )
import Data.Bool               ( Bool( False ) )
import Data.Either             ( Either( Left, Right ) )
import Data.Eq                 ( Eq )
import Data.Foldable           ( Foldable, any )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ), maybe )
import Data.String             ( String )
import System.IO.Error         ( ioeGetErrorType, userError )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Fold     ( has )
import Control.Lens.Getter   ( to )
import Control.Lens.Prism    ( Prism', prism' )
import Control.Lens.Review   ( (#) )
import System.IO.Error.Lens  ( _NoSuchThing, _PermissionDenied )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⩼) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError  ( splitMError )

-------------------------------------------------------------------------------

newtype IOError = IOErr { unErr ∷ IOException }
  deriving Eq

class AsIOError e where
  _IOError ∷ Prism' e IOError
  _IOErr   ∷ Prism' e IOException
  _IOErr   = _IOError ∘ _IOErr

instance AsIOError IOError where
  _IOError = id
  _IOErr   = prism' IOErr (Just ∘ unErr)

instance Show IOError where
  show (IOErr e) = show e

instance Exception IOError

instance Printable IOError where
  print = P.string ∘ show

----------------------------------------

userE ∷ AsIOError ε ⇒ String → ε
userE = (_IOErr #) ∘ userError

----------------------------------------

{- | raise an IOError in the IO Monad -}
ioError ∷ MonadIO μ ⇒ IOError → μ α
ioError = liftIO ∘ SysIOError.ioError ∘ unErr

----------------------------------------

{- | Is a given IOError a NoSuchThing (DoesNotExist)? -}
isNoSuchThingError ∷ AsIOError ε ⇒ ε → Bool
isNoSuchThingError = has (_IOErr ∘ to ioeGetErrorType ∘ _NoSuchThing)

----------------------------------------

{- | Is a given IOError a PermissionDenied? -}
isPermError ∷ AsIOError ε ⇒ ε → Bool
isPermError = has (_IOErr ∘ to ioeGetErrorType ∘ _PermissionDenied)

----------------------------------------

{- | Convert a function returning IO α to one that handles some IOErrors
     (e.g., fileNotExist) by returning Nothing, and otherwise returns Just α
     or rethrows other IOErrors.
-}
squashIOErrs ∷ (AsIOError ε, MonadError ε μ, Foldable φ) ⇒
               φ (IOError → Bool) → Either ε α → μ (Maybe α)
squashIOErrs ls (Left e) | maybe False (\ ps → any ($ ps) ls) (e ⩼ _IOError)
                                                              =  return Nothing
squashIOErrs _  (Left e)                                      =  throwError e
squashIOErrs _  (Right r)                                     =  return $ Just r

----------------------------------------

{- | Specialization of `squashIOErrs` to Bool; sending the positively identified
     errors to False. -}
squashIOErrsB ∷ (AsIOError ε, MonadError ε μ, Foldable φ) ⇒
                φ (IOError → Bool) → Either ε Bool → μ Bool
squashIOErrsB f = fmap (maybe False id) ∘ squashIOErrs f

{- | Given an Either IOError α (typically, a MonadError IOError μ ⇒ μ α),
     convert a 'NoSuchThing' error (e.g., DoesNotExist) to a Nothing of Maybe α.
 -}
squashNoSuchThing ∷ (AsIOError ε, MonadError ε μ) ⇒
                    Either ε α → μ (Maybe α)
squashNoSuchThing = squashIOErrs [isNoSuchThingError]


{- | `squashNoSuchThing` for `ExceptT` -}
squashNoSuchThingT ∷ (AsIOError ε, MonadError ε μ) ⇒
                     ExceptT ε μ α → μ (Maybe α)
squashNoSuchThingT = join ∘ fmap squashNoSuchThing ∘ splitMError

{- | `squashNoSuchThing` specialized to `Bool` (akin to `squashIOErrsB` -}
squashNoSuchThingB ∷ (AsIOError ε, MonadError ε μ) ⇒
                     Either ε Bool → μ Bool
squashNoSuchThingB = squashIOErrsB [isNoSuchThingError]

-- that's all, folks! ----------------------------------------------------------
