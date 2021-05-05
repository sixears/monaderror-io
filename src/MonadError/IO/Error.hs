{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module MonadError.IO.Error
  ( AsIOError(..), IOError -- hide constructor, to allow for upgrades, etc.,
  , (~~), ioeAdd
  , ioErr, ioError, isNoSuchThingError, isPermError
  , isInappropriateTypeError
  , mkIOErr
  , squashIOErrs, squashIOErrsB
  , squashInappropriateType, squashInappropriateTypeB, squashInappropriateTypeT
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
import Data.Eq                 ( Eq( (==) ) )
import Data.Foldable           ( Foldable, any )
import Data.Function           ( ($), flip, id )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, maybe )
import Data.String             ( String )
import GHC.Stack               ( CallStack, HasCallStack, callStack )
import System.IO               ( FilePath, Handle )
import System.IO.Error         ( ioeGetErrorType, ioeGetFileName, ioeGetHandle
                               , ioeGetLocation, mkIOError, userError )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Fold     ( has )
import Control.Lens.Getter   ( to )
import Control.Lens.Lens     ( lens )
import Control.Lens.Prism    ( Prism', prism' )
import Control.Lens.Review   ( (#) )
import System.IO.Error.Lens  ( _NoSuchThing, _InappropriateType
                             , _PermissionDenied )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )
import Data.MoreUnicode.Lens  ( (⩼) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError  ( splitMError )

-------------------------------------------------------------------------------

data IOError = IOErr { unErr ∷ IOException, _callstack ∷ CallStack }
  deriving Show

instance HasCallstack IOError where
  callstack = lens _callstack (\ ioe cs → ioe { _callstack = cs })

instance Eq IOError where
  (IOErr e _) == (IOErr e' _) = e == e'

--instance Show IOError where
--  show (IOErr e cs) = show e ⊕ show cs

instance Exception IOError

instance Printable IOError where
  print = P.string ∘ show ∘ unErr

ioErr ∷ HasCallStack ⇒ IOException → IOError
ioErr e = IOErr e callStack

mkIOErr ∷ IOException → CallStack → IOError
mkIOErr = IOErr

------------------------------------------------------------

class AsIOError e where
  _IOError ∷ HasCallStack ⇒ Prism' e IOError
  _IOErr   ∷ HasCallStack ⇒ Prism' e IOException
  _IOErr   = _IOError ∘ _IOErr

instance AsIOError IOError where
  _IOError = id
  _IOErr   = prism' ioErr (Just ∘ unErr)

----------------------------------------

userE ∷ (AsIOError ε, HasCallStack) ⇒ String → ε
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

{- | Given a `Foldable` of predicates that identify types of `IOError`, convert
     a possible IOError to a `MonadError` (for errors that have no matching
     predicate), a `Nothing` (for errors that do have a matching predicate) or a
     value (for non-errors).
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
squashNoSuchThing ∷ (AsIOError ε, MonadError ε μ) ⇒ Either ε α → μ (Maybe α)
squashNoSuchThing = squashIOErrs [isNoSuchThingError]

{- | `squashNoSuchThing` for `ExceptT` -}
squashNoSuchThingT ∷ (AsIOError ε, MonadError ε μ) ⇒ ExceptT ε μ α → μ (Maybe α)
squashNoSuchThingT = join ∘ fmap squashNoSuchThing ∘ splitMError

{- | `squashNoSuchThing` specialized to `Bool` (akin to `squashIOErrsB` -}
squashNoSuchThingB ∷ (AsIOError ε, MonadError ε μ) ⇒ Either ε Bool → μ Bool
squashNoSuchThingB = squashIOErrsB [isNoSuchThingError]

----------------------------------------

{- | Is a given IOError a NoSuchThing (DoesNotExist)? -}
isInappropriateTypeError ∷ AsIOError ε ⇒ ε → 𝔹
isInappropriateTypeError =
  has (_IOErr ∘ to ioeGetErrorType ∘ _InappropriateType )

{- | Given an Either IOError α (typically, a MonadError IOError μ ⇒ μ α),
     convert an 'InappropriateType' error to a Nothing of Maybe α.
 -}
squashInappropriateType ∷ (AsIOError ε, MonadError ε μ) ⇒
                          Either ε α → μ (Maybe α)
squashInappropriateType = squashIOErrs [isInappropriateTypeError]

{- | `squashInappropriateType` for `ExceptT` -}
squashInappropriateTypeT ∷ (AsIOError ε, MonadError ε μ) ⇒
                           ExceptT ε μ α → μ (Maybe α)
squashInappropriateTypeT = join ∘ fmap squashInappropriateType ∘ splitMError

{- | `squashInappropriateType` specialized to `𝔹` (akin to `squashIOErrsB` -}
squashInappropriateTypeB ∷ (AsIOError ε, MonadError ε μ) ⇒ Either ε 𝔹 → μ 𝔹
squashInappropriateTypeB = squashIOErrsB [isInappropriateTypeError]

----------------------------------------

class IOEAddable α where
  {- | Provide a default value for one of IOError's attributes; e.g., its
       filename or handle -}
  ioeAdd ∷ AsIOError ε ⇒ α → IOError → ε
  (~~) ∷ AsIOError ε ⇒ IOError → α → ε
  (~~) = flip ioeAdd

instance IOEAddable FilePath where
  ioeAdd f (IOErr e cs) =
    let e' = mkIOError (ioeGetErrorType e) (ioeGetLocation e)
                       (ioeGetHandle e) (Just $ fromMaybe f (ioeGetFileName e))
     in _IOError # IOErr e' cs

instance IOEAddable Handle where
  ioeAdd h (IOErr e cs) =
    let e' = mkIOError (ioeGetErrorType e) (ioeGetLocation e)
                       (Just $ fromMaybe h (ioeGetHandle e)) (ioeGetFileName e)
     in _IOError # IOErr e' cs


-- that's all, folks! ----------------------------------------------------------
