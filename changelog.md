0.1.1.2 2021-10-13
==================
- upgrade dependencies

0.1.1.1 2021-06-20
==================
- no more fluffy

0.1.1.0 2019-07-14
==================
- massive cleanup, and factor out into upgrades to more-unicode-0.0.6.0,
  fluffy-0.2.13.0, domainnames-0.1.0.0, hostsdb-0.1.0.0

0.1.0.4 2019-06-01
==================
- make failures such as IP dups into printed errors that do not prevent continuation

0.1.0.3 2019-05-29
==================
- where two "hosts" share an IP, and share a name but for a "-wl"; take the base name
  rather than barfing on a conflict

0.1.0.2 2019-05-28
==================
- ensure fail on failing command invocations
- ensure all Errors are Exceptions, instances of Show & Eq
- replace TempFile usage (which uses LazyIO :-() with simpler filename-only usage
- fix sixears.com.uk domain

0.1.0.1 2019-05-08
==================
- updated for Dhall, inputFrom → inputWithSettings

0.0.0.1 2019-04-05
==================
- simple working version, with inbuilt library changes
