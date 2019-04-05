{-# LANGUAGE QuasiQuotes #-}

module TinyDNS.Paths where

import Path  ( Abs, File, Path, absfile )

tinydns_edit :: Path Abs File
tinydns_edit = [absfile|__djbdns__/bin/tinydns-edit|]
