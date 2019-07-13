{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- base --------------------------------

import Control.Monad  ( return )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.ErrTs        ( ErrTs )
import Fluffy.MonadError   ( splitMError )
import Fluffy.Nat          ( One )
import Fluffy.Tasty        ( runTests_, tastyOptParser )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( HostsDomainExecCreateIOError )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

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

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  DomainNames.T.FQDN
import qualified  DomainNames.T.Hostname
import qualified  Fluffy.T.MACAddress
import qualified  HostsDB.T.Host
import qualified  HostsDB.T.Hosts
import qualified  TinyDNS.T.Hosts

import TinyDNS.Hosts              ( mkDataHosts' )
import TinyDNS.T.Hosts            ( testHosts )
import TinyDNS.Types.Clean        ( Clean( Clean ) )
import TinyDNS.Types.TinyDNSData  ( TinyDNSData )

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

tests ∷ Either HostsDomainExecCreateIOError (TinyDNSData,ErrTs) → TestTree

tests hs = testGroup "tinydns" [ DomainNames.T.FQDN.tests
                               , DomainNames.T.Hostname.tests
                               , Fluffy.T.MACAddress.tests
                               , HostsDB.T.Host.tests
                               , HostsDB.T.Hosts.tests
                               , TinyDNS.T.Hosts.tests hs
                               ]

main ∷ IO ()
main = do
  hs ← splitMError $ mkDataHosts' Clean testHosts defOptions

  tastyOpts ← customExecParser (prefs showHelpOnError) $
                info (helper ⊵ tastyOptParser (tests hs))
                     (fullDesc ⊕ progDesc "tests for tinydns package"
                               ⊕ failureCode 254)


  _ ← runTests_ tastyOpts
  return ()
