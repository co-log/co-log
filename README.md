# co-log

[![Hackage](https://img.shields.io/hackage/v/co-log.svg)](https://hackage.haskell.org/package/co-log)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/co-log/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/kowainik/co-log.svg)](https://travis-ci.org/kowainik/co-log)

Composable and configurable logging framework. The idea of the approach is
described in the following blog post:

* [co-log: Composable Contravariant Combinatorial Comonadic Configurable Convenient Logging](https://kowainik.github.io/posts/2018-09-25-co-log)

The repository contains two packages:

* [`co-log-core`](co-log-core): lightweight package with basic data types and
  general idea.
* [`co-log`](co-log): implementation of logging library based on `co-log-core`.

See [How to start using `co-log`?](co-log/README.md) for simple example of the
`co-log` library usage.

## Benchmarks

`co-log` is compared with basic functions like `putStrLn`. Since IO overhead is
big enough, every benchmark dumps 10K messages to output. If benchmark name
doesn't contain `Message` then this benchmark simply dumps string `"message"`
to output, othwerwise it works with `Message` data type from the `co-log`
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
