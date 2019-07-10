{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Options2
  ( optParser, parseOpts )
where

import Prelude  ( fromIntegral )


-- base --------------------------------

import Control.Monad       ( return, when )
import Data.Eq             ( Eq )
import Data.Function       ( ($), flip )
import Data.Maybe          ( Maybe, fromMaybe )
import Data.Word           ( Word8 )
import System.Environment  ( getProgName )
import Text.Show           ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- optparse-applicative ----------------

import Options.Applicative.Builder    ( InfoMod, failureCode, flag, fullDesc
                                      , info, long, prefs, progDesc
                                      , showHelpOnEmpty, showHelpOnError
                                      )
import Options.Applicative.Extra      ( customExecParser )
import Options.Applicative.Help.Core  ( parserHelp, parserUsage )
import Options.Applicative.Types      ( Parser, ParserPrefs )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.MonadIO   ( MonadIO, liftIO )
import Fluffy.MonadIO2  ( exitWith', warnShow )

--------------------------------------------------------------------------------

data DoHelp = DoHelp | NoHelp
  deriving (Eq, Show)

--------------------

data HelpWith α = HelpWith { _alpha ∷ α, _doHelp ∷ DoHelp }

--------------------

parseHelpWith ∷ Parser α → Parser (HelpWith α)
parseHelpWith f = HelpWith ⊳ f ⊵ flag NoHelp DoHelp (long "help")

------------------------------------------------------------

-- | standard parser preferences
parserPrefs ∷ ParserPrefs
parserPrefs = prefs $ showHelpOnError ⊕ showHelpOnEmpty

----------------------------------------

{- | Common exit code for usage errors, including --help (so that scripts that
     call --help get errors)
 -}
usageFailureCode ∷ Word8
usageFailureCode = 2

{- | Common exit code for usage errors, digestable by `Options.Applicative` -}
usageFailure ∷ InfoMod α
usageFailure = failureCode (fromIntegral usageFailureCode)

{-# DEPRECATED optParser "use `parseOpts` instead" #-}
optParser ∷ MonadIO μ ⇒ Text → Parser α → μ α
optParser descn prsr =
  let infoMod = fullDesc ⊕ progDesc (toString descn) ⊕ usageFailure
   in liftIO ∘ customExecParser parserPrefs $ info prsr infoMod


{- | Parse options, with description, helper, shows help on error and missing
     parameters.  Also exits 2 if --help is called - this is because the exit
     code is most commonly used within scripts, when calling --help is almost
     certainly not what was intended.
-}
parseOpts ∷ MonadIO μ ⇒ Maybe Text -- ^ program name (or uses `getProgName`)
                      → Text       -- ^ brief program description
                      → Parser α   -- ^ proggie opts parser
                      → μ α
parseOpts progn descn prsr = liftIO $ do
  let infoMod = fullDesc ⊕ progDesc (toString descn) ⊕ usageFailure
      prsr'   = parseHelpWith prsr
  opts ← customExecParser parserPrefs (info prsr' infoMod)
  progn' ← flip fromMaybe progn ⊳ (pack ⊳ getProgName)
  when (DoHelp ≡ _doHelp opts) $ do
    let usage = parserUsage parserPrefs prsr (unpack progn')
        help  = parserHelp  parserPrefs prsr
    warnShow usage
    warnShow help
    -- Note that usage failures, including using --help in the 'wrong' place,
    -- will result in a showHelpOnError failure; so we use the same exit code.
    exitWith' usageFailureCode
  return $ _alpha opts

-- that's all, folks! ----------------------------------------------------------
