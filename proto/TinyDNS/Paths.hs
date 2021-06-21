module TinyDNS.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

tinydns_edit :: AbsFile
tinydns_edit = [absfile|__djbdns__/bin/tinydns-edit|]
