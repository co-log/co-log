# Changelog

`co-log` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.7.0.0 - April 6, 2025

## What's Changed

* Move away from `chronos`. (#277)

  **Breaking change**: Some signatures have changed from `Chronos.Time` to `Data.Time.UTCTime`.

**Full Changelog**: https://github.com/co-log/co-log/compare/v0.6.1.2...v0.7.0.0

## 0.6.1.2 - March 2, 2025

## What's Changed

* Allow `containers-0.8`.

**Full Changelog**: https://github.com/co-log/co-log/compare/v0.6.1.1...v0.6.1.2

## 0.6.1.1 - January 5, 2025

## What's Changed

* Support ghc-9.10 & ghc-9.12.

**Full Changelog**: https://github.com/co-log/co-log/compare/v0.6.1.0...v0.6.1.1

## 0.6.1.0 - Mar 1, 2024

## What's Changed

* GA(deps): Bump actions/cache from 3 to 4 by @dependabot in https://github.com/co-log/co-log/pull/273
* docs: refine the loggert tutorials by @xieyuschen in https://github.com/co-log/co-log/pull/272
* Support ghc-9.8. by @alaendle in https://github.com/co-log/co-log/pull/270

**Full Changelog**: https://github.com/co-log/co-log/compare/v0.6.0.2...v0.6.1.0

## 0.6.0.0 - Sep 18, 2023

### What's Changed

* Support GHC-9.6 - replace `typerep-map` with `dependent-map`. by @alaendle in [#264](https://github.com/co-log/co-log/pull/264)
* Support GHC 9.4. by @alaendle in [#252](https://github.com/co-log/co-log/pull/252)
* Add MonadUnliftIO instance by @newhoggy in [#240](https://github.com/co-log/co-log/pull/240)
* Update CI tested GHC versions & workaround for GHC < 9.4.5 (run-st, primitive-unlifted) by @alaendle in [#257](https://github.com/co-log/co-log/pull/257)
* Derive `MonadFail` for `LoggerT` by @alaendle in [#260](https://github.com/co-log/co-log/pull/260)
* docs: use relative path to benefit locally reading by @xieyuschen in [#253](https://github.com/co-log/co-log/pull/253)
* docs: refine readme by @xieyuschen in [#254](https://github.com/co-log/co-log/pull/254)
* tutorials: add demo for LoggerT and SimpleMsg, #84 by @xieyuschen in [#256](https://github.com/co-log/co-log/pull/256)
* docs: add a link to tutorial pages and aggregate all tutorial pages by @xieyuschen in [#259](https://github.com/co-log/co-log/pull/259)
* tutorials: add a tutorial for loggert and message by @xieyuschen in [#261](https://github.com/co-log/co-log/pull/261)
* Create tags and upload package candidates on version bumps. by @alaendle in [#265](https://github.com/co-log/co-log/pull/265)
* Added @alaendle as code owner. by @alaendle in [#258](https://github.com/co-log/co-log/pull/258)
* GA(deps): Bump actions/checkout from 3 to 4 by @dependabot in [#263](https://github.com/co-log/co-log/pull/263)

### New Contributors

* @xieyuschen made their first contribution in [#253](https://github.com/co-log/co-log/pull/253)
* @newhoggy made their first contribution in [#240](https://github.com/co-log/co-log/pull/240)

**Full Changelog**: https://github.com/co-log/co-log/compare/v0.5.0.0...v0.6.0.0

## 0.5.0.0 - Nov 2, 2022

* [#230](https://github.com/co-log/co-log/issues/230):
  Support GHC-9.2.
* Allow `mtl-2.3`.
* Allow `vector-0.13`.
* Allow `hedgehog-1.2`.
* [#187](https://github.com/co-log/co-log/issues/187):
  Remove `CoLog.Concurrent` module and executable.
* [#243](https://github.com/co-log/co-log/pull/243):
  Improve printing in multiple threads.
* Drop support for GHC-8.2, GHC-8.4, GHC-8.6, GHC-8.8

## 0.4.0.2 — <M> <d>, 2021

* [#223](https://github.com/co-log/co-log/pulls/223):
  Support GHC-9.0.1.
  Require typerep-map ^>= 0.4

## 0.4.0.1 — Apr 18, 2020

* [#186](https://github.com/co-log/co-log/issues/186):
  Support GHC-8.10.1.

## 0.4.0.0 — Jan 19, 2020

* [#120](https://github.com/co-log/co-log/issues/120):
  Improve time formatting.

  Old: `29-12-2019 22:00:00.000`

  New: `29 Dec 2019 22:00:00.000 +00:00`

  (by [@vrom911](https://github.com/vrom911))
* [#144](https://github.com/co-log/co-log/issues/144):
  Add Windows CI check.
  (by [@vrom911](https://github.com/vrom911))
* [#148](https://github.com/co-log/co-log/issues/148):
  Support GHC-8.8.2.
  (by [@chshersh](https://github.com/chshersh))
* [#119](https://github.com/co-log/co-log/issues/119):
  Add new message type that allows printing messages without
  `Severity`.
  (by [@sphaso](https://github.com/sphaso))
* [#150](https://github.com/co-log/co-log/issues/150):
  Introduce `formatWith` — beginner-friendly alias for formatting combinator.
  (by [@chshersh](https://github.com/chshersh))
* Use `chronos-1.1` as `1.0.9` is not Windows-compatible.
  (by [@vrom911](https://github.com/vrom911))
* [#156](https://github.com/co-log/co-log/issues/156):
  Improve documentation for the `Colog.Concurrent` module.
  (by [@chshersh](https://github.com/chshersh))
* [#146](https://github.com/co-log/co-log/issues/146):
  Allow `ansi-terminal-0.10`.
  (by [@mpilgrem](https://github.com/mpilgrem))
* [#124](https://github.com/co-log/co-log/issues/124):
  Implement executable playground for concurrent logging.
  (by [@chshersh](https://github.com/chshersh))

## 0.3.0.0 — May 5, 2019

* [#77](https://github.com/co-log/co-log/issues/77):
  **Important:** Use `chronos` time formatter. This is a breaking change because
  default field map in `RichMessage` now contains different type representing
  time. If you use your custom formatter for time, you should change it.
  Othwerwise no observable differences in the library API usage will be noticed.
* [#103](https://github.com/co-log/co-log/issues/103):
  **Breaking change:** make `Message` data type polymorhic over the type of severity.

  **Migration guide:** this change is done in backwards-compatible way. If you
  use any fields of the `Message` data type, you should rename them according to
  the following scheme:
  ```haskell
  messageSeverity -> msgSeverity
  messageStack    -> msgStack
  messageText     -> msgText
  ```
* Export more formatting functions to make implementation of custom formatters easier.
* [#96](https://github.com/co-log/co-log/issues/96):
  Add `simpleMessageAction` and `richMessageAction` to work with `Message`s.
* Use `co-log-core` of version `0.2.0.0`.

## 0.2.0 — Nov 15, 2018

* [#45](https://github.com/co-log/co-log/issues/45):
  Introduce approach for concurrent log writing.
* [#46](https://github.com/co-log/co-log/issues/46):
  Moves `logStringStdout`, `logStringStderr`, `logStringHandle`,
  `withLogStringFile` from `Colog.Actions` to `Colog.Core.IO`
* [#77](https://github.com/co-log/co-log/issues/77):
  Remove `relude` from dependencies.
  Add HLint check to Travis CI.
* [#64](https://github.com/co-log/co-log/issues/64):
  Introduce basic benchmarks.
* [#20](https://github.com/co-log/co-log/issues/20):
  Add experimental support for logger rotation (see `Colog.Rotation` module).
* [#39](https://github.com/co-log/co-log/issues/39):
  Support GHC-8.2.2 and GHC-8.6.2.

## 0.1.0

* [#37](https://github.com/co-log/co-log/issues/37):
  Add bounds to all dependencies. Move `Prelude` to the
  `other-modules` section.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/co-log/co-log/releases
