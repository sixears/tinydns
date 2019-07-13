{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TinyDNS.Edit
  ( addAlias, addAliases, addHost, addHosts, addMx, addNS, addNSen )
where

-- base --------------------------------

import Control.Monad           ( foldM, return, when )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Foldable           ( Foldable, toList )
import Data.Function           ( ($), flip )
import Data.List               ( sortOn )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Tuple              ( snd, uncurry )

-- base-unicode-functions --------------

import Data.Eq.Unicode        ( (≡) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- domainnames -------------------------

import DomainNames.FQDN      ( FQDN )
import DomainNames.Hostname  ( Hostname )

-- fluffy ------------------------------

import Fluffy.IO.Error      ( AsIOError )
import Fluffy.IP4           ( IP4 )
import Fluffy.MonadIO       ( unlink_ )
import Fluffy.MonadIO.File  ( readFile, writeFile )
import Fluffy.Path          ( parseAbsFile_ )
import Fluffy.TempFile      ( mktempf )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks )
import Control.Monad.Trans   ( lift )

-- path --------------------------------

import Path  ( absfile, relfile )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )
import ProcLib.Process                ( mkIO, mkIO', mkProc_ )
import ProcLib.Types.CmdSpec          ( CmdArgs, CmdSpec( CmdSpec ) )
import ProcLib.Types.CreateProcOpts   ( MockLvl( MockLvl ) )
import ProcLib.Types.ProcIO           ( ProcIO )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  TinyDNS.Paths  as  Paths

import TinyDNS.Types.Clean        ( HasClean( clean ), Clean( Clean ) )
import TinyDNS.Types.TinyDNSData  ( TinyDNSData( TinyDNSData ) )

--------------------------------------------------------------------------------

tinydnsEdit ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
               MonadIO μ, HasClean α, MonadReader α μ) ⇒
              CmdArgs → TinyDNSData → ProcIO ε μ TinyDNSData

tinydnsEdit args intxt = do
  cl ← lift (asks $ view clean)
  let mock1 = [absfile|/tmp/mktinydns.data|]
      mock2 = [absfile|/tmp/mktinydns.data.tmp|]
  (tmp1,tmp2) ← mkIO' (MockLvl 1) (mock1,mock2) "mktmpnames" $ do
    tmpfn1 ← mktempf [absfile|/tmp/tinydns-data-|] Nothing
    tmpfn2 ← mktempf [absfile|/tmp/tinydns-data-|] (Just [relfile|.tmp|])
    let tmp1 = parseAbsFile_ tmpfn1
        tmp2 = parseAbsFile_ tmpfn2
    return (tmp1,tmp2)

  mkIO ([fmt|write: %T|] tmp1) $ writeFile tmp1 (toText intxt)
  mkProc_ @_ @() $ CmdSpec Paths.tinydns_edit ([toText tmp1,toText tmp2] ⊕ args)
  outtxt ← mkIO ([fmt|read: %T|] tmp1) $ readFile tmp1

  -- tmp2 is auto-deleted by tinydns-data
  when (cl ≡ Clean) $
       mkIO ("clean: " ⊕ toText tmp1) $ unlink_ tmp1 -- ⪼ unlink_ tmp2

  return (TinyDNSData outtxt)

----------------------------------------

addNS ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
         MonadIO μ, HasClean α, MonadReader α μ) ⇒
        FQDN → IP4 → TinyDNSData → ProcIO ε μ TinyDNSData

addNS d ip =
  tinydnsEdit [ "add", "ns", toText d, toText ip ]

--------------------

addNSen ∷ (Foldable ψ, Foldable φ, AsCreateProcError ε, AsExecError ε,
           AsIOError ε, MonadIO μ, HasClean α, MonadReader α μ) ⇒
          φ FQDN → ψ IP4 → TinyDNSData → ProcIO ε μ TinyDNSData

addNSen ds ips tinydnsdata = 
  let go t ds' i = foldM (\ t' d → addNS d i t') t ds'
   in foldM (\ t i → go t ds i) tinydnsdata ips

----------------------------------------

addHost ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
           MonadIO μ, HasClean α, MonadReader α μ) ⇒
          Hostname → IP4 → TinyDNSData → ProcIO ε μ TinyDNSData
addHost hn ip =
  tinydnsEdit [ "add", "host", toText hn, toText ip ]

--------------------

addHosts ∷ (Foldable ψ, AsCreateProcError ε, AsExecError ε, AsIOError ε,
            MonadIO μ, HasClean α, MonadReader α μ) ⇒
           ψ (Hostname,IP4) → TinyDNSData → ProcIO ε μ TinyDNSData

addHosts hostList t =
  foldM (flip $ uncurry addHost) t (sortOn snd $ toList hostList)

----------------------------------------

addAlias ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
            MonadIO μ, HasClean α, MonadReader α μ) ⇒
           Hostname → IP4 → TinyDNSData → ProcIO ε μ TinyDNSData
addAlias name ip =
  tinydnsEdit [ "add", "alias", toText name, toText $ ip ]

--------------------

addAliases ∷ (Foldable ψ, AsCreateProcError ε, AsExecError ε, AsIOError ε,
              MonadIO μ, HasClean α, MonadReader α μ) ⇒
             ψ (Hostname,IP4) → TinyDNSData → ProcIO ε μ TinyDNSData
addAliases as t = foldM (flip $ uncurry addAlias) t as

----------------------------------------

addMx ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
         MonadIO μ, HasClean α, MonadReader α μ) ⇒
        Hostname → IP4 → TinyDNSData → ProcIO ε μ TinyDNSData
addMx hname ip = tinydnsEdit [ "add", "mx", toText hname, toText ip ]

-- that's all, folks! ----------------------------------------------------------
