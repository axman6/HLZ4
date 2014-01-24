{-# LANGUAGE BangPatterns #-}

module Codec.Compression.HLZ4.Decode
    -- (
    -- decompress
    -- ) 
    where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr)
import Foreign.Storable (peekByteOff, peek, poke)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils as F (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8, Word16, Word32)
import Data.Bits ((.|.), (.&.), shiftR, testBit)
import Control.Monad (when)
type P8 = Ptr Word8
type PtrState = (P8, P8, Int, Int)

-- | A PtrParser is a monad which keeps track of the src and dst arrays
-- and how many bytes are left in both
newtype PtrParser a = PtrParser (PtrState -> IO (Either String (PtrState, a)))

runPtrParser :: PtrParser a -> PtrState -> IO (Either String (PtrState, a))
runPtrParser (PtrParser f) s = f s




instance Monad PtrParser where
    return a = PtrParser $ \s -> return $ Right (s,a)
    (PtrParser m) >>= f  = PtrParser $ \s -> do
        x <- m s
        case x of
            Right (s', a) -> let PtrParser f' = f a in f' s'
            Left str      -> return $ Left str
    fail str = PtrParser $ \_ -> return $ Left str


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


getSrcRemaining :: PtrParser Int
getSrcRemaining = PtrParser $ \s@(_,_,srem,_) -> return $ Right (s,srem)

getDestRemaining :: PtrParser Int
getDestRemaining = PtrParser $ \s@(_,_,_,drem) -> return $ Right (s,drem)

peekByte :: Ptr Word8 -> Int -> IO Word8
peekByte = peekByteOff

getWord8 :: PtrParser Word8
getWord8 = PtrParser $ \s@(src,dst,srem,drem) -> do
    b <- peek src
    return $ Right (advanceSrcState 1 s,b)
{-# INLINE getWord8 #-}

getWord16LE :: PtrParser Word16
getWord16LE = PtrParser $ \s@(src,_,_,_) -> do
    a <- fromIntegral `fmap` peekByte src 0
    b <- fromIntegral `fmap` peekByte src 1
    return $ Right (advanceSrcState 2 s,(a `shiftR` 8 .|. b))
{-# INLINE getWord16LE #-}


getWord32LE :: PtrParser Word32
getWord32LE = PtrParser $ \s@(src,_,_,_) -> do
    a <- fromIntegral `fmap` peekByte src 0
    b <- fromIntegral `fmap` peekByte src 1
    c <- fromIntegral `fmap` peekByte src 2
    d <- fromIntegral `fmap` peekByte src 3
    return $ Right (advanceSrcState 4 s,(a `shiftR` 24 .|. b `shiftR` 16 .|. c `shiftR` 8 .|. d))
{-# INLINE getWord32LE #-}


setDestination :: Int -> PtrParser ()
setDestination n = PtrParser $ \(src, _, srem, _) -> do
    dst <- mallocBytes n
    return $ Right ((src, dst, srem, n), ())

-- | Decodes a number encoded using repeated values of 255 and returns how many bytes were used
getLength :: PtrParser Int
getLength = PtrParser $ \s@(src,_,srem,_) -> do
    let go !p 0  !_  = return $ Left "getLength: reached end of source."
        go  p sr n   =  do
            b <- peek p
            case b :: Word8 of
                255 -> go (p `plusPtr` 1) (sr-1) (n+255)
                _ -> return $ Right (n + fromIntegral b, srem-(sr-1))
    
    r <- go src srem 0
    return $! case r of
        Right (n,len) -> Right (advanceSrcState len s, n)
        Left str      -> Left str    
{-# INLINE getLength #-}

-- | Moves `count` bytes of data from `offset` bytes before current position into
-- the current position in the destination, and advances the state. Unchecked.
transfer :: Int -> PtrParser ()
transfer count = PtrParser $ \s@(src,dst,_,_) -> do
    F.copyBytes dst src count
    return $ Right (advanceBothState count s,())

-- | Moves `count` bytes of data from `offset` bytes before current position into
-- the current position in the destination, and advances the state. Unchecked.
lookback :: Int -> Int -> PtrParser ()
lookback count offset = PtrParser $ \s@(_,dst,_,_) -> do
    let mmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
        mmove dest src 0 = return ()
        mmove dest src n = peek src >>= poke dest >> mmove (dest `plusPtr` 1) (src `plusPtr` 1) (n-1)
    mmove dst (dst `plusPtr` (-offset)) count
    return $ Right (advanceDestState count s,())


getSrcProgress :: Int -> PtrParser Int
getSrcProgress n = PtrParser $ \s@(_,_,srem,_) -> return $ Right (s, n-srem)

getDestProgress :: Int -> PtrParser Int
getDestProgress n = PtrParser $ \s@(_,_,_,drem) -> return $ Right (s, n-drem)


-- | Decodes a single LZ4 sequence within a block (lit len, lits, offset backwards, copy len).
-- Returns the number of bytes 
decodeSequence :: PtrParser ()
decodeSequence = do
    ensureSrc 1
    token <- getWord8
    let lLen = fromIntegral $ (token .&. 0xF0) `shiftR` 4
        mLen = fromIntegral $ (token .&. 0x0F) + 4
    litLength <- if lLen == 15 
                then (15+) `fmap` getLength 
                else return lLen
    ensureSrc  litLength
    ensureDest litLength
    transfer   litLength
    b <- atSrcEnd
    if b
        then return ()
        else do
        offset <- getWord16LE
        matchLen <- if mLen == 19
            then (19+) `fmap` getLength
            else return mLen
        ensureDest matchLen
        lookback matchLen (fromIntegral offset)
    
getBlock :: PtrParser ()
getBlock = do
    len <- getWord32LE
    if testBit len 31 then do
        ensureBoth (fromIntegral len)
    else
            undefined
    



