-- | Module:      Acme.LOLCAT
--   Copyright:   (c) 2013 Anton Nikishaev
--   License:     BSD
--   Maintainer:  me@lelf.lu

module Acme.LOLCAT.IO (OH(..),HAI(..),I(..),CAN(..),HAZ(..),
                       IO(..),YO(..),(?),THXBYE(..)) where

import Prelude hiding (IO)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad

data HAI a b c d e = HAI a b c d e
data OH a b c d e = OH a b c d e
data I = I
data IO = IO
data YO = YO
data CAN = CAN
data HAZ = HAZ
data THXBYE = THXBYE

_?_ = return unsafePerformIO

