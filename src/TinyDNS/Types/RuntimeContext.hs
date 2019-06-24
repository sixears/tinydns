{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TinyDNS.Types.RuntimeContext
  ( RuntimeContext( RuntimeContext ) )
where

-- base --------------------------------

import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- hostsdb -----------------------------

import HostsDB.Hosts  ( HasHosts( hosts ), Hosts )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Types.Clean  ( HasClean( clean ), Clean )

--------------------------------------------------------------------------------

data RuntimeContext = RuntimeContext { _clean_ ∷ Clean
                                     , _hosts_ ∷ Hosts
                                     }
  deriving (Eq, Show)

instance HasClean RuntimeContext where
  clean = lens _clean_ (\ rc cl → rc { _clean_ = cl })

instance HasHosts RuntimeContext where
  hosts = lens _hosts_ (\ rc hs → rc { _hosts_ = hs })

-- that's all, folks! ----------------------------------------------------------
