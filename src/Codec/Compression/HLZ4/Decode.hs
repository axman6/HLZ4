{-# LANGUAGE BangPatterns #-}

module Codec.Compression.HLZ4.Decode
    -- (
    -- decompress
    -- ) 
    where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr)
import Foreign.Storable (peekByteOff, peek, poke)
import Foreign.Marshal.Utils as F (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8, Word16, Word32)
import Data.Bits ((.|.), (.&.), shiftR, testBit)
