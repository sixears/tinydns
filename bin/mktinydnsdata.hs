-- base --------------------------------

import Control.Monad           ( forM_, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.Word               ( Word8 )
import GHC.Stack               ( HasCallStack )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- dhall-plus --------------------------

import DhallPlus.Error  ( AsDhallError )

-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- hostsdb -----------------------------

import HostsDB.Hosts             ( loadFile )
import HostsDB.Error.HostsError  ( AsHostsError )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO  ( liftIO, warn )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )

-- stdmain -----------------------------

import StdMain  ( stdMainNoDR' )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Error.MkTinyDNSError         ( MkTinyDNSError )
import TinyDNS.Hosts                        ( mkDataHosts )
import TinyDNS.Types.MkTinyDNSData.Options  ( Options, input, parseOptions )
import TinyDNS.Types.Clean                  ( HasClean( clean ) )

--------------------------------------------------------------------------------

myMain ∷ ∀ ε μ .
         (MonadIO μ, HasCallStack,
          AsIOError ε, AsDhallError ε, AsDomainError ε, AsHostsError ε,
          AsFPathError ε, AsExecError ε, AsCreateProcError ε, MonadError ε μ) ⇒
         Options → μ Word8
myMain opts = do
  hs ← loadFile (opts ⊣ input)

  (t,es) ← mkDataHosts (opts ⊣ clean) hs opts
  liftIO $ TextIO.putStr (toText t)
  forM_ es $ warn ∘ ("!ERROR: " ⊕)
  return 0


main ∷ IO ()
{-
main = let go = flip $ const ∘ myMain @MkTinyDNSError
        in stdMain' none "make tinydns data from hosts config" parseOptions go
-}
main = let progDesc = "make tinydns data from hosts config"
        in stdMainNoDR' progDesc parseOptions (myMain @MkTinyDNSError)

-- that's all, folks! ----------------------------------------------------------
