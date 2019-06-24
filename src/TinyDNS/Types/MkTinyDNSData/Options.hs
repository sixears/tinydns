{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UnicodeSyntax         #-}

module TinyDNS.Types.MkTinyDNSData.Options
  ( Options( Options ), input )
where

-- base --------------------------------

-- fluffy ------------------------------

import Fluffy.Nat   ( One )
import Fluffy.Path  ( AbsFile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( DryRun, HasDryRunLevel( dryRunLevel ) )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel ), Verbose )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Types.Clean  ( HasClean( clean ), Clean )

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

-- that's all, folks! ----------------------------------------------------------
