{-# LANGUAGE QuasiQuotes #-}

module TinyDNS.Paths where

import Path  ( Abs, File, Path, absfile )

tinydns_edit :: Path Abs File
tinydns_edit = [absfile|/nix/store/jc270sbssqqar6sx47y8lzd769497dr2-djbdns-1.05/bin/tinydns-edit|]
