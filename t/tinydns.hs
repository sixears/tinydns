{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- monaderror-io -----------------------

import MonadError   ( splitMError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Text         ( 𝕋 )

-- natural -----------------------------

import Natural  ( One )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( failureCode, fullDesc, info, prefs
                                    , progDesc, showHelpOnError )
import Options.Applicative.Extra    ( customExecParser, helper )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( DryRun, HasDryRunLevel( dryRunLevel )
                                  , dryRunOff )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel ), Verbose
                                  , verboseOff )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus        ( runTests_, tastyOptParser )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  TinyDNS.T.Hosts

import TinyDNS.Error.MkTinyDNSError  ( MkTinyDNSError )
import TinyDNS.Hosts                 ( mkDataHosts )
import TinyDNS.T.Hosts               ( testHosts )
import TinyDNS.Types.Clean           ( Clean( Clean ) )
import TinyDNS.Types.TinyDNSData     ( TinyDNSData )

--------------------------------------------------------------------------------

data Options = Options { _dryRun   ∷ DryRun
                       , _verbose  ∷ Verbose
                       }

defOptions ∷ Options
defOptions = Options dryRunOff verboseOff

dryRun ∷ Lens' Options DryRun
dryRun = lens _dryRun (\ o d → o { _dryRun = d })

instance HasDryRunLevel One Options where
  dryRunLevel = dryRun

verbose ∷ Lens' Options Verbose
verbose = lens _verbose (\ o v → o { _verbose = v })

instance HasVerboseLevel One Options where
  verboseLevel = verbose

------------------------------------------------------------

tests ∷ Either MkTinyDNSError (TinyDNSData,[𝕋]) → TestTree

tests hs = testGroup "tinydns" [ TinyDNS.T.Hosts.tests hs ]

main ∷ IO ()
main = do
  hs ← splitMError $ mkDataHosts Clean testHosts defOptions

  tastyOpts ← customExecParser (prefs showHelpOnError) $
                info (helper ⊵ tastyOptParser (tests hs))
                     (fullDesc ⊕ progDesc "tests for tinydns package"
                               ⊕ failureCode 254)


  _ ← runTests_ tastyOpts
  return ()
