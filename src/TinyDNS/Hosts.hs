{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TinyDNS.Hosts
  ( {- | integration of tinydns cmds with HostsDB -}
    mkAliasCmds, mkData, mkDataHosts, mkDataHosts', mkMxCmds, mkNSCmds
  )
where

-- base --------------------------------

import Control.Monad  ( foldM, forM, mapM, return )
import Data.Function  ( ($), flip )

-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError )
import DomainNames.Hostname           ( (<..>) )

-- fluffy ------------------------------

import Fluffy.ErrTs     ( ErrTs )
import Fluffy.IO.Error  ( AsIOError )
import Fluffy.MonadIO   ( MonadIO )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( AsHostsError, HostsDomainExecCreateIOError )
import HostsDB.Host              ( hname, ipv4 )
import HostsDB.Hosts             ( HasHosts
                                 , aliasHosts, dnsServers, hosts, hostIPs
                                 , hostIPv4
                                 , inAddr, lookupHost, mailServers, subDomain
                                 )

-- lens --------------------------------

import Control.Lens.Getter   ( view )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Monoid2  ( ф )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Reader  ( MonadReader, asks, runReaderT )
import Control.Monad.Trans   ( lift )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun       ( HasDryRunLevel )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel )
import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )
import ProcLib.Process                ( doProcIO )
import ProcLib.Types.ProcIO           ( ProcIO )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Edit                  ( addAliases, addHosts, addMx, addNSen )
import TinyDNS.Types.Clean           ( HasClean( clean ) )
import TinyDNS.Types.RuntimeContext  ( RuntimeContext( RuntimeContext ) )
import TinyDNS.Types.TinyDNSData     ( TinyDNSData )

--------------------------------------------------------------------------------

mkNSCmds ∷ (AsCreateProcError ε, AsExecError ε, AsHostsError ε, AsIOError ε,
            MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒
           TinyDNSData → ProcIO ε μ TinyDNSData
mkNSCmds tinydnsdata = do
  hs  ← lift (asks $ view hosts)
  ips ← lift $ mapM (hostIPv4 hs) (hs ⊣ dnsServers)
  addNSen [hs ⊣ subDomain, hs ⊣ inAddr] ips tinydnsdata

----------------------------------------

mkAliasCmds ∷ (AsCreateProcError ε, AsExecError ε,
               AsDomainError ε, AsHostsError ε, AsIOError ε,
               MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒
              TinyDNSData → ProcIO ε μ TinyDNSData
mkAliasCmds t = do
  hs ← lift (asks $ view hosts)
  as ← lift (HashMap.toList ⊳ aliasHosts hs)
  let aliasHostIP (l,h) = (,h ⊣ ipv4) ⊳ l <..> (hs ⊣ subDomain)
  aliases ← lift $ forM as aliasHostIP
  addAliases aliases t

----------------------------------------

mkMxCmds ∷ (AsExecError ε, AsCreateProcError ε, AsHostsError ε, AsIOError ε,
            MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒
           TinyDNSData → ProcIO ε μ TinyDNSData

mkMxCmds tinydnsdata = do
  hs ← lift (asks $ view hosts)
  let go t h = do mx ← lift $ lookupHost hs h
                  addMx (mx ⊣ hname) (mx ⊣ ipv4) t
  foldM (\ t h → go t h) tinydnsdata (hs ⊣ mailServers)

----------------------------------------

mkData ∷ (AsHostsError ε, AsExecError ε, AsCreateProcError ε,
          AsDomainError ε, AsIOError ε,
          MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒

         ProcIO ε μ (TinyDNSData, ErrTs)

mkData = do
  hs ← lift $ asks (view hosts)
  let (hostList,es) = hostIPs hs

  tinydnsdata ← mkNSCmds ф ≫ addHosts hostList ≫ mkAliasCmds ≫ mkMxCmds

  return (tinydnsdata,es)

----------------------------------------

mkDataHosts ∷ (HasDryRunLevel υ θ, HasVerboseLevel ν θ, MonadIO μ,
               MonadError ε μ, AsIOError ε, AsDomainError ε,
               AsCreateProcError ε, AsExecError ε, AsHostsError ε,
               HasClean ρ, HasHosts σ) ⇒
              ρ → σ → θ → μ (TinyDNSData,ErrTs)
mkDataHosts c hs o = let rContext = RuntimeContext (c ⊣ clean) (hs ⊣ hosts)
                      in flip runReaderT rContext $ doProcIO o mkData

mkDataHosts' ∷ (HasDryRunLevel υ θ, HasVerboseLevel ν θ, MonadIO μ,
                MonadError HostsDomainExecCreateIOError μ,
                HasClean ρ, HasHosts σ) ⇒
               ρ → σ → θ → μ (TinyDNSData,ErrTs)
mkDataHosts' = mkDataHosts

-- that's all, folks! ----------------------------------------------------------
