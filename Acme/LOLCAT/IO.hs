-- | Module:      Acme.LOLCAT
--   Copyright:   (c) 2013 Antonio Nikishaev
--   License:     BSD
--   Maintainer:  me@lelf.lu
-- 
-- >>> (OH HAI I CAN HAZ ЙO? THXBYE <*> putStrLn) "LOL" == ()
-- LOL
-- True



{-# LANGUAGE Unsafe #-}

module Acme.LOLCAT.IO (OH(..),HAI(..),I(..),CAN(..),HAZ(..),
                       IO(..),ÏO(..),ЙО(..),ЙO(..),YO(..),
                       (?),THXBYE(..)) where

import Prelude hiding (IO)
import System.IO.Unsafe (unsafePerformIO)


data HAI a b c d e = HAI a b c d e
data OH a b c d e = OH a b c d e
data I = I
data IO = IO
data ÏO = ÏO
data ЙО = ЙО
data ЙO = ЙO
data YO = YO
data CAN = CAN
data HAZ = HAZ
data THXBYE = THXBYE

_?_ = return unsafePerformIO

