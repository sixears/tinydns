{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import Control.Monad           ( forM_, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Nothing ) )
import Data.Word               ( Word8 )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError )

-- fluffy ------------------------------

import Fluffy.Dhall.Error  ( AsDhallError )
import Fluffy.ErrTs        ( toTexts )
import Fluffy.IO.Error     ( AsIOError )
import Fluffy.Main         ( doMain )
import Fluffy.MonadIO      ( warn, writeOut )
import Fluffy.Options      ( parseOpts )
import Fluffy.Path         ( getCwd_ )

-- hostsdb -----------------------------

import HostsDB.Hosts             ( loadFile )
import HostsDB.Error.HostsError  ( AsHostsError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )
import Data.MoreUnicode.Monad  ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Error.MkTinyDNSError         ( MkTinyDNSError )
import TinyDNS.Hosts                        ( mkDataHosts )
import TinyDNS.Types.MkTinyDNSData.Options  ( input, parseOptions )
import TinyDNS.Types.Clean                  ( HasClean( clean ) )

--------------------------------------------------------------------------------

myMain ∷ (AsIOError ε, AsDhallError ε, AsDomainError ε, AsHostsError ε,
          AsExecError ε, AsCreateProcError ε, MonadError ε μ, MonadIO μ) ⇒
         μ Word8
myMain = do
  let descn = "make tiny dns data from hosts config"
  opts ← getCwd_ ≫ parseOpts Nothing descn ∘ parseOptions

  hs ← loadFile (opts ⊣ input)

  (t,es) ← mkDataHosts (opts ⊣ clean) hs opts
  writeOut (toText t)
  forM_ (toTexts es) $ warn ∘ ("!ERROR: " ⊕)
  return 0

main ∷ IO ()
main = doMain $ myMain @MkTinyDNSError

-- that's all, folks! ----------------------------------------------------------
