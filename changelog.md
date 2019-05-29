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
