{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UnicodeSyntax         #-}

module TinyDNS.Types.MkTinyDNSData.Options
  ( Options( Options ), input, parseOptions )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just ) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode    ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Nat    ( One )
import Fluffy.Path   ( AbsDir, AbsFile, readAbsFile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, flag, help, long, metavar )
import Options.Applicative.Types    ( Parser )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( DryRun, HasDryRunLevel( dryRunLevel )
                                  , dryRunP )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel ), Verbose
                                  , verboseP )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Types.Clean  ( HasClean( clean ), Clean( Clean, NoClean ) )

--------------------------------------------------------------------------------

data Options = Options { _dryRun   ∷ DryRun
                       , _verbose  ∷ Verbose
                       , _input    ∷ AbsFile
                       , _clean    ∷ Clean
                       }

dryRun ∷ Lens' Options DryRun
dryRun = lens _dryRun (\ o d → o { _dryRun = d })

instance HasDryRunLevel One Options where
  dryRunLevel = dryRun

verbose ∷ Lens' Options Verbose
verbose = lens _verbose (\ o v → o { _verbose = v })

instance HasVerboseLevel One Options where
  verboseLevel = verbose

input ∷ Lens' Options AbsFile
input = lens _input (\ o i → o { _input = i })

instance HasClean Options where
  clean = lens _clean (\ o d → o { _clean = d })

----------------------------------------

parseOptions ∷ AbsDir → Parser Options
parseOptions cwd =
  let cleanHelpText = "don't delete intermediate files"
   in Options ⊳ dryRunP
              ⊵ verboseP
              ⊵ argument (readAbsFile $ Just cwd) (metavar "HOSTS.dhall")
              ⊵ flag Clean NoClean (long "no-clean" ⊕ help cleanHelpText)

-- that's all, folks! ----------------------------------------------------------
