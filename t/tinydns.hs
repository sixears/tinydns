{-# LANGUAGE UnicodeSyntax #-}

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.ErrTs        ( ErrTs )
import Fluffy.Tasty        ( runTests_, tastyOptParser )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( HostsDomainExecCreateIOError )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( failureCode, fullDesc, info, prefs
                                    , progDesc, showHelpOnError )
import Options.Applicative.Extra    ( customExecParser, helper )

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

import TinyDNS.T.Hosts            ( mkDataHosts, testHosts )
import TinyDNS.Types.TinyDNSData  ( TinyDNSData )

--------------------------------------------------------------------------------

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
  hs ← mkDataHosts testHosts

  tastyOpts ← customExecParser (prefs showHelpOnError) $
                info (helper ⊵ tastyOptParser (tests hs))
                     (fullDesc ⊕ progDesc "tests for tinydns package"
                               ⊕ failureCode 254)


  runTests_ tastyOpts

