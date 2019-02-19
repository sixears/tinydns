{-# LANGUAGE QuasiQuotes #-}

module TinyDNS.Paths where

import Path  ( Abs, File, Path, absfile )

tinydns_data :: Path Abs File
tinydns_data = [absfile|__djbdns__/bin/tinydns-data|]
