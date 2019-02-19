{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.TempFile2
  ( pc, pathComponent
  , withTempFile, withTempFile', with2TempFiles, with2TempFiles'
  , withTempFileContents, withTempFileContents' )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad  ( join )
import Data.Bool      ( otherwise )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe, maybe )
import Data.Tuple     ( curry )
import System.IO      ( Handle, IO, hClose )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- mtl --------------------------------

import Control.Monad.Except  ( MonadError )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ, appE, conE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter( QuasiQuoter, quoteDec
                                               , quoteExp, quotePat, quoteType )
                                  )


-- temporary ---------------------------

import System.IO.Temp  ( withSystemTempFile )

-- text --------------------------------

import Data.Text     ( Text, any, null, pack, unpack )
import Data.Text.IO  ( hPutStr )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.IO.Error       ( AsIOError, IOError )
import Fluffy.Monad          ( (⪼) )
import Fluffy.MonadIO        ( MonadIO )
import Fluffy.MonadError     ( splitMError )
import Fluffy.MonadError.IO  ( asIOError )
import Fluffy.Path           ( AbsFile, parseAbsFile_ )
import Fluffy.Printable      ( __ERR, q )

--------------------------------------------------------------------------------

{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent Text

instance Show PathComponent where
  show (PathComponent t) = "[pathComponent|" ⊕ unpack t ⊕ "|]"

instance Printable PathComponent where
  print (PathComponent t) = P.text t

qPathC ∷ Text -> ExpQ
qPathC t | null t         = error "empty pathComponent"
         | any (≡ '\0') t = __ERR $ "pathComponent contains NUL: "   ⊕ q t
         | any (≡ '/')  t = __ERR $ "pathComponent contains SLASH: " ⊕ q t
         | otherwise      = appE (conE 'PathComponent)
                                 (appE (varE 'pack) $ litE (stringL $ unpack t))

{- | quasi-quoter for PathComponent -}
pathComponent ∷ QuasiQuoter
pathComponent =
  QuasiQuoter { quoteDec  = error "pathComponent quoteDec  not implemented"
              , quoteType = error "pathComponent quoteType not implemented"
              , quotePat  = error "pathComponent quotePat  not implemented"
              , quoteExp  = qPathC ∘ pack
              }

{- | abbreviation for `pathComponent` -}
pc ∷ QuasiQuoter
pc = pathComponent

{- | see `System.IO.Temp.openTempFile` for use of template (prefix) argument,
     particularly around the use of "extensions", which includes anything
     involving a '.' character
 -}
withTempFile ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
               Maybe PathComponent → ((AbsFile,Handle) → IO α) → μ α
withTempFile pfx io = asIOError $ withSystemTempFile (maybe "" toString pfx)
                                                     (curry io ∘ parseAbsFile_)

withTempFile' ∷ (MonadIO μ, MonadError IOError μ) =>
                Maybe PathComponent → ((AbsFile,Handle) → IO α) → μ α
withTempFile' = withTempFile

withTempFileContents ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) =>
                       Maybe PathComponent → Text → (AbsFile → IO α) → μ α
withTempFileContents pfx txt io =
  withTempFile pfx ( \ (f,h) → hPutStr h txt ⪼ hClose h ⪼ io f )

withTempFileContents' ∷ (MonadIO μ, MonadError IOError μ) =>
                        Maybe PathComponent → Text → (AbsFile → IO α) → μ α
withTempFileContents' = withTempFileContents

with2TempFiles ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                 Maybe PathComponent → Maybe PathComponent
               → ((AbsFile,Handle) → (AbsFile,Handle) → IO α)
               → μ α
with2TempFiles pfx1 pfx2 io =
  join $
    withTempFile pfx1 ( \ (fn,h) → splitMError $ withTempFile pfx2 (io (fn,h)))

with2TempFiles' ∷ (MonadIO μ, MonadError IOError μ) =>
                  Maybe PathComponent → Maybe PathComponent
                → ((AbsFile,Handle) → (AbsFile,Handle) → IO α)
                → μ α
with2TempFiles' = with2TempFiles

-- that's all, folks! ----------------------------------------------------------
