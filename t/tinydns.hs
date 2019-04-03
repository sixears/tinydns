{-# LANGUAGE UnicodeSyntax #-}

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Tasty  ( runTests_, tastyOptParser )

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

--------------------------------------------------------------------------------

tests ∷ TestTree

tests = testGroup "tinydns" [ DomainNames.T.FQDN.tests
                            , DomainNames.T.Hostname.tests
                            , Fluffy.T.MACAddress.tests
                            , HostsDB.T.Host.tests
                            , HostsDB.T.Hosts.tests
                            ]

main ∷ IO ()
main = do
  tastyOpts ← customExecParser (prefs showHelpOnError) $
                info (helper ⊵ tastyOptParser tests)
                     (fullDesc ⊕ progDesc "tests for tinydns package"
                               ⊕ failureCode 254)

  runTests_ tastyOpts

