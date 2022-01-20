module MonadError.IO.Error
  ( AsIOError(..), IOError -- hide constructor, to allow for upgrades, etc.,
  , annotateIOE
  , (~~), ioeAdd
  , ioeErrorString, ioeFilename, ioeHandle, ioeLocation, ioeType
  , ioErr, ioError, isNoSuchThingError, isPermError
  , isInappropriateTypeError
  , mkIOErr
  , squashIOErrs, squashIOErrsB
  , squashInappropriateType, squashInappropriateTypeB, squashInappropriateTypeT
  , squashNoSuchThing, squashNoSuchThingT, squashNoSuchThingB, userE
  )
where

import Base0

-- base --------------------------------

import qualified  System.IO.Error  as  SysIOError

import Control.Exception.Base  ( IOException )
import Data.Bool               ( Bool( False ) )
import Data.Foldable           ( Foldable, any )
import Data.Function           ( flip )
import Data.Maybe              ( fromMaybe )
import GHC.Generics            ( Generic )
import System.IO               ( FilePath, Handle )
import System.IO.Error         ( IOErrorType
                               , ioeGetErrorString, ioeGetErrorType
                               , ioeGetFileName, ioeGetHandle, ioeGetLocation
                               , ioeGetErrorType, mkIOError, userError
                               )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData( rnf ), rwhnf )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Fold     ( has )
import Control.Lens.Getter   ( to )
import System.IO.Error.Lens  ( _NoSuchThing, _InappropriateType
                             , _PermissionDenied )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Either   ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⩼), (⨦) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.String   ( 𝕊 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError  ( modifyError, splitMError )

-------------------------------------------------------------------------------

type ℍ = Handle

------------------------------------------------------------

data IOError = IOErr { unErr ∷ IOException, _callstack ∷ CallStack }
  deriving (Generic,Show)

instance NFData IOError where
  rnf (IOErr ioe cs) = rnf (rwhnf ioe, rnf cs)

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
  _IOErr   = prism' ioErr (𝕵 ∘ unErr)

----------------------------------------

userE ∷ (AsIOError ε, HasCallStack) ⇒ String → ε
userE = (_IOErr #) ∘ userError

----------------------------------------

{- | raise an IOError in the IO Monad -}
ioError ∷ MonadIO μ ⇒ IOError → μ α
ioError = liftIO ∘ SysIOError.ioError ∘ unErr

----------------------------------------

ioeFilename ∷ AsIOError ε ⇒ ε → 𝕄 FilePath
ioeFilename e = join $ ioeGetFileName ⊳ e ⩼ _IOErr

----------------------------------------

ioeHandle ∷ AsIOError ε ⇒ ε → 𝕄 ℍ
ioeHandle e = join $ ioeGetHandle ⊳ e ⩼ _IOErr

----------------------------------------

ioeType ∷ AsIOError ε ⇒ ε → 𝕄 IOErrorType
ioeType e = ioeGetErrorType ⊳ e ⩼ _IOErr

----------------------------------------

ioeLocation ∷ AsIOError ε ⇒ ε → 𝕄 𝕊
ioeLocation e = ioeGetLocation ⊳ e ⩼ _IOErr

----------------------------------------

ioeErrorString ∷ AsIOError ε ⇒ ε → 𝕄 𝕊
ioeErrorString e = ioeGetErrorString ⊳ e ⩼ _IOErr

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
               φ (IOError → Bool) → 𝔼 ε α → μ (𝕄 α)
squashIOErrs ls (𝕷 e) | maybe False (\ ps → any ($ ps) ls) (e ⩼ _IOError)
                                                              =  return 𝕹
squashIOErrs _  (𝕷 e)                                         =  throwError e
squashIOErrs _  (𝕽 r)                                         =  return $ 𝕵 r

----------------------------------------

{- | Specialization of `squashIOErrs` to Bool; sending the positively identified
     errors to False. -}
squashIOErrsB ∷ (AsIOError ε, MonadError ε μ, Foldable φ) ⇒
                φ (IOError → Bool) → 𝔼 ε Bool → μ Bool
squashIOErrsB f = fmap (maybe False id) ∘ squashIOErrs f

{- | Given an Either IOError α (typically, a MonadError IOError μ ⇒ μ α),
     convert a 'NoSuchThing' error (e.g., DoesNotExist) to a Nothing of Maybe α.
 -}
squashNoSuchThing ∷ (AsIOError ε, MonadError ε μ) ⇒ 𝔼 ε α → μ (𝕄 α)
squashNoSuchThing = squashIOErrs [isNoSuchThingError]

{- | `squashNoSuchThing` for `ExceptT` -}
squashNoSuchThingT ∷ (AsIOError ε, MonadError ε μ) ⇒ ExceptT ε μ α → μ (𝕄 α)
squashNoSuchThingT = join ∘ fmap squashNoSuchThing ∘ splitMError

{- | `squashNoSuchThing` specialized to `Bool` (akin to `squashIOErrsB` -}
squashNoSuchThingB ∷ (AsIOError ε, MonadError ε μ) ⇒ 𝔼 ε Bool → μ Bool
squashNoSuchThingB = squashIOErrsB [isNoSuchThingError]

----------------------------------------

{- | Is a given IOError a NoSuchThing (DoesNotExist)? -}
isInappropriateTypeError ∷ AsIOError ε ⇒ ε → 𝔹
isInappropriateTypeError =
  has (_IOErr ∘ to ioeGetErrorType ∘ _InappropriateType )

{- | Given an Either IOError α (typically, a MonadError IOError μ ⇒ μ α),
     convert an 'InappropriateType' error to a Nothing of Maybe α.
 -}
squashInappropriateType ∷ (AsIOError ε, MonadError ε μ) ⇒ 𝔼 ε α → μ (𝕄 α)
squashInappropriateType = squashIOErrs [isInappropriateTypeError]

{- | `squashInappropriateType` for `ExceptT` -}
squashInappropriateTypeT ∷ (AsIOError ε, MonadError ε μ) ⇒
                           ExceptT ε μ α → μ (𝕄 α)
squashInappropriateTypeT = join ∘ fmap squashInappropriateType ∘ splitMError

{- | `squashInappropriateType` specialized to `𝔹` (akin to `squashIOErrsB` -}
squashInappropriateTypeB ∷ (AsIOError ε, MonadError ε μ) ⇒ 𝔼 ε 𝔹 → μ 𝔹
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
                       (ioeGetHandle e) (𝕵 $ fromMaybe f (ioeGetFileName e))
     in _IOError # IOErr e' cs

instance IOEAddable ℍ where
  ioeAdd h (IOErr e cs) =
    let e' = mkIOError (ioeGetErrorType e) (ioeGetLocation e)
                       (𝕵 $ fromMaybe h (ioeGetHandle e)) (ioeGetFileName e)
     in _IOError # IOErr e' cs

----------------------------------------

{- | Given a lens into a maybe field of an `IOException`; update the field
     with a given value iff that field is `𝕹`.-}
annotateIOE ∷ ∀ ε β α (η ∷ * → *) .
              (MonadError ε η, AsIOError ε) =>
              Lens' IOException (𝕄 α) → α → ExceptT ε η β → η β
annotateIOE f x = modifyError (\ e → e & _IOErr ∘ f ⨦ x)

-- that's all, folks! ----------------------------------------------------------
