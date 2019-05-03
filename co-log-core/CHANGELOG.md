# Change log

`co-log-core` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## Unreleased

* [#85](https://github.com/kowainik/co-log/issues/85):
  Move `overLogAction` to `HasLog` typeclass
* [#101](https://github.com/kowainik/co-log/issues/101):
  Add `logActionL` lens with default implementation to `HasLog` type class.

## 0.1.1 — Nov 15, 2018

* [#63](https://github.com/kowainik/co-log/issues/63):
  Add `logPrint`, `logPrintStderr`, `logPrintHandle` and `withLogPrintFile` to `Colog.Core.IO`.
* [#46](https://github.com/kowainik/co-log/issues/46):
  Moves `logStringStdout`, `logStringStderr`, `logStringHandle`,
  `withLogStringFile` from `Colog.Actions` to `Colog.Core.IO`.
* [#48](https://github.com/kowainik/co-log/issues/48):
  Adds `liftLogIO` function.
* [#49](https://github.com/kowainik/co-log/issues/49):
  Add `<&` and `&>`operators for `unLogAction`.
* [#47](https://github.com/kowainik/co-log/issues/47):
  Add `doctest` tests.
* [#13](https://github.com/kowainik/co-log/issues/13):
  Add `.cabal` file description and improve documentation.
* [#39](https://github.com/kowainik/co-log/issues/39):
  Support GHC-8.2.2 and GHC-8.6.2.

## 0.1.0

* [#38](https://github.com/kowainik/co-log/issues/38):
  Rename `cbind` to `cmapM`.

* [#37](https://github.com/kowainik/co-log/issues/37):
  Add `base` bounds.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/co-log/releases
