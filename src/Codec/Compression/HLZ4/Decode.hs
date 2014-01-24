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
type P8 = Ptr Word8
type PtrState = (P8, P8, Int, Int)

-- | A PtrParser is a monad which keeps track of the src and dst arrays
-- and how many bytes are left in both
newtype PtrParser a = PtrParser (PtrState -> IO (Either String (PtrState, a)))

instance Monad PtrParser where
    return a = PtrParser $ \s -> return $ Right (s,a)
    (PtrParser m) >>= f  = PtrParser $ \s -> do
        x <- m s
        case x of
            Right (s', a) -> let PtrParser f' = f a in f' s'
            Left str      -> return $ Left str


instance Functor PtrParser where
    fmap f (PtrParser m) = PtrParser $ \s -> do
        ex <- m s
        return $! case ex of
            Right (s',x) -> Right (s, f x)
            Left str     -> Left str

ensureSrc :: Int -> PtrParser ()
ensureSrc n = PtrParser $ \s@(_,_,srem,_) -> 
    return $ if n >= srem && srem >= 0 
                then Right (s,())
                else Left $ "ensureSrc: not enough bytes available in source. " ++ show (n, srem)


ensureDest :: Int -> PtrParser ()
ensureDest n = PtrParser $ \s@(_,_,_,drem) -> 
    return $ if n >= drem && drem >= 0
                then Right (s,()) 
                else Left $ "ensureDest: not enough bytes available in destination. " ++ show (n,drem)

ensureBoth :: Int -> PtrParser ()
ensureBoth n = PtrParser $ \s@(_,_,srem,drem) -> 
    return $ if n >= srem && srem >= 0
                then if n >= drem && drem >= 0
                        then Right (s,()) 
                        else Left $ "ensureBoth: not enough bytes available in destination. " ++ show (n,drem)
                else Left $ "ensureBoth: not enough bytes available in source. " ++ show (n, srem)

atSrcEnd :: PtrParser Bool
atSrcEnd = PtrParser $ \s@(_,_,srem,_) -> return $ Right (s,srem == 0)

advanceSrcState :: Int -> PtrState -> PtrState
advanceSrcState n (src,dst,srem,drem) =
    (src `plusPtr` n, dst, srem - n, drem)

advanceDestState :: Int -> PtrState -> PtrState
advanceDestState n (src,dst,srem,drem) =
    (src, dst `plusPtr` n, srem, drem - n)


advanceBothState :: Int -> PtrState -> PtrState
advanceBothState n (src,dst,srem,drem) =
    (src `plusPtr` n, dst `plusPtr` n, srem - n, drem - n)


advanceSrc :: Int -> PtrParser ()
advanceSrc n = PtrParser $ \s ->
    return $ Right (advanceSrcState n s,())

advanceDest :: Int -> PtrParser ()
advanceDest n = PtrParser $ \s ->
    return $ Right (advanceDestState n s,())
