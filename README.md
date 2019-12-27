# co-log

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/co-log/blob/master/LICENSE)
[![Build status](https://img.shields.io/travis/kowainik/co-log.svg?logo=travis)](https://travis-ci.org/kowainik/co-log)

|                   |                                    |                                           |                                                       |
| :------------     | :--------------------------------- | :---------------------------------------- | :---------------------------------------------------- |
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

See the following tutorial series about the library:

* [Intro: Using `LogAction`](https://github.com/kowainik/co-log/blob/master/co-log/tutorials/1-intro/Intro.md)
* [Using custom monad that stores `LogAction` inside its environment](https://github.com/kowainik/co-log/blob/master/co-log/tutorials/2-custom/Custom.md)

`co-log` also cares about concurrent logging. For this purposes we have the `concurrent-playground`
executable where we experiment with different multithreading scenarios to test the library behavior.
You can find it here:

* [tutorials/Concurrent.hs](co-log/tutorials/Concurrent.hs)

## Benchmarks

`co-log` is compared with basic functions like `putStrLn`. Since IO overhead is
big enough, every benchmark dumps 10K messages to output. If benchmark name
doesn't contain `Message` then this benchmark simply dumps string `"message"`
to output, otherwise it works with `Message` data type from the `co-log`
library.

| Benchmarks                                              | Time for 10K messages |
| :------------------------------------------------------ | :-------------------- |
| `Prelude.putStrLn`                                      | `  5.117ms`           |
| `Text.putStrLn`                                         | `  9.220ms`           |
| `ByteString.putStrLn`                                   | `  2.971ms`           |
| `mempty`                                                | `  1.181ms`           |
| `logStringStdout`                                       | `  5.107ms`           |
| `logPrint`                                              | `  5.248ms`           |
| `logTextStdout`                                         | `  5.351ms`           |
| `logByteStringStdout`                                   | `  2.933ms`           |
| `logByteStringStderr`                                   | ` 17.482ms`           |
| `ByteString > (stdout <> stderr)`                       | ` 17.715ms`           |
| `Message > format > stdout`                             | `  9.188ms`           |
| `Message > format > ByteString > stdout`                | `  3.524ms`           |
| `Message{callstack} > format > stdout`                  | `  9.139ms`           |
| `Message{callstack:5} > format > stdout`                | `  9.464ms`           |
| `Message{callstack:50} > format > stdout`               | `  9.439ms`           |
| `Message{Time,ThreadId} > format > stdout`              | ` 54.160ms`           |
| `Message{Time,ThreadId} > format > ByteString > stdout` | ` 54.137ms`           |


[hk-img]: https://img.shields.io/hackage/v/co-log.svg?logo=haskell
[hk-img-ps]: https://img.shields.io/hackage/v/co-log-polysemy.svg?logo=haskell
[hk-img-core]: https://img.shields.io/hackage/v/co-log-core.svg?logo=haskell
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
