# Change log

`co-log` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## Unreleased: 0.3.0

* [#77](https://github.com/kowainik/co-log/issues/77):
  Use chronos time formatter.

## 0.2.0 — Nov 15, 2018

* [#45](https://github.com/kowainik/co-log/issues/45):
  Introduce approach for concurrent log writing.
* [#46](https://github.com/kowainik/co-log/issues/46):
  Moves `logStringStdout`, `logStringStderr`, `logStringHandle`,
  `withLogStringFile` from `Colog.Actions` to `Colog.Core.IO`
* [#77](https://github.com/kowainik/co-log/issues/77):
  Remove `relude` from dependencies.
  Add HLint check to Travis CI.
* [#64](https://github.com/kowainik/co-log/issues/64):
  Introduce basic benchmarks.
* [#20](https://github.com/kowainik/co-log/issues/20):
  Add experimental support for logger rotation (see `Colog.Rotation` module).
* [#39](https://github.com/kowainik/co-log/issues/39):
  Support GHC-8.2.2 and GHC-8.6.2.

## 0.1.0

* [#37](https://github.com/kowainik/co-log/issues/37):
  Add bounds to all dependencies. Move `Prelude` to the
  `other-modules` section.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/co-log/releases
