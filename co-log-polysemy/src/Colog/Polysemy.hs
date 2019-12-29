{- |
Copyright:  (c) 2019-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This package contains extensible effects implementation of composable, contravariant and
comonadic logging based on @co-log-core@ and @polysemy@.
-}

module Colog.Polysemy
       ( module Poly
       ) where

import Colog.Polysemy.Effect as Poly
