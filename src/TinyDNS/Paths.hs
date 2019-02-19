{-# LANGUAGE QuasiQuotes #-}

module TinyDNS.Paths where

import Path  ( Abs, File, Path, absfile )

tinydns_edit :: Path Abs File
tinydns_edit = [absfile|/nix/store/mf5sdil14kv047g29wgbvgk2sp22111j-djbdns-1.05/bin/tinydns-edit|]
