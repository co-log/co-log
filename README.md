# co-log

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/co-log/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/kowainik/co-log.svg)](https://travis-ci.org/kowainik/co-log)

|                   |                                    |                                           |                                                       |
| :------------     | :--------------------------------- | :---------------------------------------: | :---------------------------------------------------- |
| `co-log-core`     | [![Hackage][hk-img-core]][hk-core] | [![Stackage LTS][lts-img-core]][lts-core] | [![Stackage Nightly][nightly-img-core]][nightly-core] |
| `co-log`          | [![Hackage][hk-img]][hk]           | [![Stackage LTS][lts-img]][lts]           | [![Stackage Nightly][nightly-img]][nightly]           |
| `co-log-polysemy` | [![Hackage][hk-img-ps]][hk-ps]     | [![Stackage LTS][lts-img-ps]][lts-ps]     | [![Stackage Nightly][nightly-img-ps]][nightly-ps]     |

`co-log` is a composable and configurable logging framework. The idea of the approach is
described in the following blog post:

* [co-log: Composable Contravariant Combinatorial Comonadic Configurable Convenient Logging](https://kowainik.github.io/posts/2018-09-25-co-log)

The repository contains the following packages:

* [`co-log-core`](co-log-core): lightweight package with basic data types and
  general idea.
* [`co-log`](co-log): taggless final implementation of logging library based on
  `co-log-core`.
* [`co-log-polysemy`](co-log-polysemy): implementation of logging library based
  on `co-log-core` and the [`polysemy`](http://hackage.haskell.org/package/polysemy) extensible effects library.
* [`co-log-benchmark`](co-log-benchmark): Benchmarks of the `co-log` library.

See [How to start using `co-log`?](co-log/README.md) for simple example of the
`co-log` library usage.

## Benchmarks

`co-log` is compared with basic functions like `putStrLn`. Since IO overhead is
big enough, every benchmark dumps 10K messages to output. If benchmark name
doesn't contain `Message` then this benchmark simply dumps string `"message"`
to output, otherwise it works with `Message` data type from the `co-log`
library.

| Benchmarks                                 | Time        |
| :----------------------------------------- | :---------- |
| `Prelude.putStrLn`                         | `  5.358ms` |
| `Text.putStrLn`                            | `  9.255ms` |
| `ByteString.putStrLn`                      | `  2.970ms` |
| `mempty`                                   | `  1.229ms` |
| `logStringStdout`                          | `  5.127ms` |
| `logPrint`                                 | `  5.163ms` |
| `logTextStdout`                            | `  5.140ms` |
| `logByteStringStdout`                      | `  3.931ms` |
| `logByteStringStderr`                      | ` 17.503ms` |
| `ByteString > (stdout <> stderr)`          | ` 17.612ms` |
| `Message > format > stdout`                | `  9.367ms` |
| `Message > format > ByteString > stdout`   | `  2.994ms` |
| `Message{callstack} > format > stdout`     | `  9.367ms` |
| `Message{callstack:5} > format > stdout`   | `  9.441ms` |
| `Message{callstack:50} > format > stdout`  | `  9.305ms` |
| `Message{Time,ThreadId} > format > stdout` | ` 53.870ms` |


[hk-img]: https://img.shields.io/hackage/v/co-log.svg
[hk-img-ps]: https://img.shields.io/hackage/v/co-log-polysemy.svg
[hk-img-core]: https://img.shields.io/hackage/v/co-log-core.svg
[hk]: https://hackage.haskell.org/package/co-log
[hk-ps]: https://hackage.haskell.org/package/co-log-polysemy
[hk-core]: https://hackage.haskell.org/package/co-log-core
[lts-img]: http://stackage.org/package/co-log/badge/lts
[lts-img-ps]: http://stackage.org/package/co-log-polysemy/badge/lts
[lts-img-core]: http://stackage.org/package/co-log-core/badge/lts
[lts]: http://stackage.org/lts/package/co-log
[lts-ps]: http://stackage.org/lts/package/co-log-polysemy
[lts-core]: http://stackage.org/lts/package/co-log-core
[nightly-img]: http://stackage.org/package/co-log/badge/nightly
[nightly-img-ps]: http://stackage.org/package/co-log-polysemy/badge/nightly
[nightly-img-core]: http://stackage.org/package/co-log-core/badge/nightly
[nightly]: http://stackage.org/nightly/package/co-log
[nightly-ps]: http://stackage.org/nightly/package/co-log-polysemy
[nightly-core]: http://stackage.org/nightly/package/co-log-core
