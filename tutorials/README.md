# Co-log Tutorial Series

This directory contains examples of using `co-log` library written as literate Haskell tutorials. Considering some prefer to read the source code directly, there are `.hs` files inside the corresponding folder.


|No|Tutorial|Brief Introduction|
|--|--|--|
|1|[LogAction: start to use co-log](1-intro/Intro.md)|In this tutorial, you will learn how to replace the `putStrLn` and `print` with `LogAction`|
|2|[Simple message and LoggerT](2-loggert/README.md)|This tutorial will show you how to use `LoggerT` and Simple message to log with more information in a more flexiable way.|
|3|[Using custom monad that stores `LogAction` inside its environment](2-custom/Custom.md)|This tutorial will show you how to custom monad to store `LogAction` inside its environment.|
|4|[Collection of all common usages](Main.hs)|The code list all common usages about co-log, could check it to find out what you like.|
