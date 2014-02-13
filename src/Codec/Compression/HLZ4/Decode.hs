{-# LANGUAGE BangPatterns, ExistentialQuantification, 
    TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Codec.Compression.HLZ4.Decode
    -- (
    -- decompress
    -- ) 
    where


import Control.Monad (when)
import Control.Monad.IO.Class

import Control.Applicative ((<$>))

import Data.Bits ((.|.), (.&.), shiftR, shiftL, testBit, clearBit)

import Data.ByteString as BS (ByteString, pack, drop, length)
import Data.ByteString.Internal (toForeignPtr, fromForeignPtr)

import Data.Word (Word8, Word16, Word32)

import Foreign.ForeignPtr (touchForeignPtr, newForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
import Foreign.Marshal.Utils as F (copyBytes)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (peekByteOff, peek, poke)

import Text.Printf

debug :: Bool
debug = False

type P8 = Ptr Word8
-- | tuple of src and dest pointers, offset into and their lengths
type PtrState = (P8, Int, Int, P8, Int, Int)

advanceSrcState :: Int -> PtrState -> PtrState
advanceSrcState n (src, soff, slen, dst, doff, dlen) =
    (src, soff+n, slen, dst, doff, dlen)

advanceDestState :: Int -> PtrState -> PtrState
advanceDestState n (src, soff, slen, dst, doff, dlen) =
    (src, soff, slen, dst, doff + n, dlen)


advanceBothState :: Int -> PtrState -> PtrState
advanceBothState n (src, soff, slen, dst, doff, dlen) =
    (src, soff+n, slen, dst, doff + n, dlen)



newtype PtrParser a = PtrParser (PtrState -> IO (PtrState, PPResult a))

runPtrParser :: PtrParser a -> PtrState -> IO (PtrState, PPResult a)
runPtrParser (PtrParser p) = p

data PPResult a
    = PPDone a
            -- ^ returns the src ptr and the length remaining, and a pointer to 
    | PPPartial (PtrParser a)
        -- ^ More input is required, pass in a Ptr Word8 and the length in bytes
    | PPSetDest Int (PtrParser a)
        -- ^ Allocate a new destination Ptr of the given size and continue parsing with the provided parser.
    | PPError String
        -- ^ Something bad happened

instance Show a => Show (PPResult a) where
    show x = case x of
        PPDone a -> "PPDone " ++ show a
        PPError str -> "PPError: " ++ str
        PPSetDest n _ -> "PPSetDest " ++ show n
        PPPartial _ -> "PPPartial <p>"


instance Functor PtrParser where
    fmap f m = m >>= return . f



instance Monad PtrParser where
    return a = PtrParser $ \s -> return (s,PPDone a)
    PtrParser m >>= f = PtrParser $ \s -> do
        (s',r) <- m s
        case r of
            PPDone x   ->
                let PtrParser fx = f x in fx s'
            PPPartial g  ->
                return $ (s',PPPartial (g >>= f))
            PPSetDest len p ->
                return (s',PPSetDest len (p >>= f))
            PPError str  ->
                return (s',PPError str)

instance MonadIO PtrParser where
    liftIO m = PtrParser $ \s -> do
        x <- m
        return (s,PPDone x)
    
setSrc :: (P8,Int,Int) -> PtrParser ()
setSrc (src,soff,slen) = do 
    PtrParser $ \(_,_,_,dst,doff,dlen) ->
        return ((src,soff,slen,dst,doff,dlen),PPDone ())


demandInput :: PtrParser ()
demandInput = PtrParser $ \s -> return (s,PPPartial (return ()))

getState :: PtrParser PtrState
getState = PtrParser $ \s -> return (s, PPDone s)

putState :: PtrState -> PtrParser ()
putState s = PtrParser $ \_s -> return (s,PPDone ())

modifyState :: (PtrState -> PtrState) -> PtrParser ()
modifyState f = PtrParser $ \s -> return (f s,PPDone ())


startPParse :: Show a => PtrParser a -> ByteString -> IO HLZ4Result
startPParse p bs = do
   let (srcptr, off, len) = toForeignPtr bs
   r <- runPP bs (unsafeForeignPtrToPtr srcptr, off, len, nullPtr, 0, 0) p
   touchForeignPtr srcptr
   return r



runPP :: Show a => ByteString -> PtrState -> PtrParser a -> IO HLZ4Result
runPP input s (PtrParser p) = do
    (s'@(src,soff,slen,dst,doff,dlen),x) <- p s
    when debug $ print x
    case x of
        PPDone _  -> do
            fptr <- newForeignPtr finalizerFree dst
            let res = (fromForeignPtr fptr 0 doff)
            if (soff == slen)
                then return $ Done res
                else return $ Block res (BS.drop soff input)
                    
        PPPartial g -> 
            return $ Partial (feed s' g)
        PPSetDest len' p' -> do
            dst' <- mallocBytes len'
            runPP input (src,soff,slen,dst',0,len') p'
        PPError str -> return $ Error str


feed :: Show a => PtrState -> PtrParser a -> ByteString -> IO HLZ4Result
feed (_,_,_,dst,doff,dlen) g bs = do
    let (srcptr, off, len) = toForeignPtr bs
    r <- runPP bs (unsafeForeignPtrToPtr srcptr, off, len,dst,doff,dlen) g
    touchForeignPtr srcptr
    return r


data HLZ4Result
    = Done ByteString -- ^ Finished decoding all there is
    | Block ByteString ByteString -- ^ Decoded a block, and remiaing input
    | Partial (ByteString -> IO HLZ4Result) -- ^ More data is needed to decompress
    | Error String -- ^ Something bad happened

instance Show HLZ4Result where
    show x = case x of
        Done bs -> "Done " ++ show bs
        Block b1 b2 -> "Block " ++ show b1 ++ " (" ++ show b2 ++ ")"
        Error str -> "Error: " ++ str
        Partial _ -> "Partial"



err :: String -> PtrParser a
err str = PtrParser $ \s -> return $ (s,PPError str)



getSrcRemaining :: PtrParser Int
getSrcRemaining = getState >>= \(_,soff,slen,_,_,_) -> return (slen-soff)

getDestRemaining :: PtrParser Int
getDestRemaining = getState >>= \(_,_,_,_,doff,dlen) -> return (dlen-doff)




advanceSrc :: Int -> PtrParser ()
advanceSrc n = modifyState(advanceSrcState n)

advanceDest :: Int -> PtrParser ()
advanceDest n = modifyState(advanceDestState n)

advanceBoth :: Int -> PtrParser ()
advanceBoth n = modifyState(advanceBothState n)


peekByte :: Ptr Word8 -> Int -> IO Word8
peekByte = peekByteOff



getWord8 :: PtrParser Word8
getWord8 = do
    (src,soff,slen,_,_,_) <- getState
    if soff < slen
        then do
            b <- liftIO $ peekByte src soff
            advanceSrc 1
            when debug $ liftIO $ printf "getWord8: %d\n" b
            return b
        else demandInput >> getWord8 
{-# INLINE getWord8 #-}

getWord16LE :: PtrParser Word16
getWord16LE = do
    a' <- getWord8
    b' <- getWord8
    let [a,b] = map fromIntegral [a',b']
    return (b `shiftL` 8 .|. a)
{-# INLINE getWord16LE #-}


getWord32LE :: PtrParser Word32
getWord32LE = do
    a' <- getWord8
    b' <- getWord8
    c' <- getWord8
    d' <- getWord8
    let [a,b,c,d] = map fromIntegral [a',b',c',d']
    return (d `shiftL` 24 .|. c `shiftL` 16 .|. b `shiftL` 8 .|. a)
{-# INLINE getWord32LE #-}
 

allocateDest :: Int -> PtrParser ()
allocateDest n =
    PtrParser $ \s-> do
        return (s, PPSetDest n (return ()))


-- | Decodes a number encoded using repeated values of 255 and returns how many bytes were used
getLength :: PtrParser Int
getLength = go 0 where
    go n = do
        b <- getWord8
        case b of
            255 -> go (n+255)
            _   -> return (n + fromIntegral b)   
{-# INLINE getLength #-}

-- | Transfers `count` bytes from src into dest. If there are not enough
-- bytes in src, the remaining bytes will be copied and more input demanded.
-- If there is not enough room in the destination and error is produced.
transfer :: Int -> PtrParser ()
transfer count = do
    (src,soff,slen,dst,doff,dlen) <- getState
    let srem = slen - soff
    case () of
        _ | soff + count < slen && doff + count < dlen -> do
                liftIO $ F.copyBytes (dst `plusPtr` doff) (src `plusPtr` soff) count
                advanceBoth count
          | soff + count > slen && doff + count < dlen -> do
                liftIO $ F.copyBytes (dst `plusPtr` doff) (src `plusPtr` soff) srem
                advanceDest srem
                demandInput
                transfer (count-srem)
          | otherwise -> err $ "transfer: transfer of " 
                              ++ show count
                              ++ " bytes would overflow destination buffer "
                              ++ show (doff,dlen)


-- | Moves `count` bytes of data from `offset` bytes before current position into
-- the current position in the destination, and advances the state. If count is greater
-- than the space remaining in the destination buffer, an error is returned.
lookback :: Int -> Int -> PtrParser ()
lookback count offset = do
    (_,_,_,dst,doff,dlen) <- getState
    when (doff + count > dlen) $ 
        err $ "lookback: copy of " ++ show count ++ " bytes would overflow destination buffer"
    when (offset > doff) $
        err $ "lookback: copy from offset " ++ show offset ++ " before beginning of buffer, dest offset: " ++ show doff
    let mmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
        mmove !_ !_ 0 = return ()
        mmove dest src n = peek src >>= poke dest >> mmove (dest `plusPtr` 1) (src `plusPtr` 1) (n-1)
    
    liftIO $ mmove (dst `plusPtr` doff) (dst `plusPtr` (doff-offset)) count
    advanceDest count


getByteString :: Int -> PtrParser ByteString
getByteString len = do
    bsptr <- liftIO $ mallocBytes len
    go bsptr len
    fptr <- liftIO $ newForeignPtr finalizerFree bsptr
    return $ fromForeignPtr fptr 0 len

    where go ptr len = do
            (src,soff,slen,_,_,_) <- getState
            if soff + len < slen
                then do
                    liftIO $ F.copyBytes ptr (src `plusPtr` soff) len
                    advanceSrc len
                else do
                    let srem = slen-soff
                    liftIO $ F.copyBytes ptr (src `plusPtr` soff) srem
                    demandInput
                    go (ptr `plusPtr` srem) (len-srem)

-- | Decodes a single LZ4 sequence within a block (lit len, lits, offset backwards, copy len).
-- Returns the number of bytes written to the destination
decodeSequence :: PtrParser Int
decodeSequence = do
    token <- getWord8
    let lLen = fromIntegral $ token `shiftR` 4
        mLen = fromIntegral $ (token .&. 0x0F) + 4
    -- Write literals
    litLength <- if lLen == 15 
                then (15+) <$> getLength 
                else return lLen
    transfer litLength
    
    -- copy length from offset
    drem <- getDestRemaining
    if drem > 0 then do
            offset <- getWord16LE
            matchLen <- if mLen == 19
                then (19+) <$> getLength
                else return mLen
            lookback matchLen (fromIntegral offset)
        return (litLength + matchLen)
    else
        return litLength

decodeSequences :: Int -> PtrParser ()
decodeSequences len 
    | len < 0   = err $ "decodeSequence: read more than block length bytes from source: " ++ show len
    | len == 0  = return ()
    | otherwise = do
        ssize <- decodeSequence
        decodeSequences (len-ssize)
    
getBlock :: PtrParser ()
getBlock = do
    len' <- getWord32LE
    let len = fromIntegral (len' `clearBit` 31)
    when debug $ liftIO $ print len'
    allocateDest len
    if testBit len' 31
        then transfer len
        else decodeSequences len
        
-- Tests --

test1 :: IO Bool
test1 = do
    r <- startPParse getBlock  $ pack [15,0,0,0, 0x56, 49, 50,51,52,53, 5,0]
    case r of
        Done bs -> return $ bs == pack [49, 50,51,52,53,49, 50,51,52,53,49, 50,51,52,53]
        _ -> return False
 

test2 :: IO Bool
test2 = do
    r <- startPParse getBlock  $ pack [15,0,0,0, 0x56, 49, 50,51,52,53, 5,0,0,0,0,0]
    case r of
        Block bs res -> return $ bs  == pack [49, 50,51,52,53,49, 50,51,52,53,49, 50,51,52,53]
                              && res == pack [0,0,0,0]
        _ -> return False
 
test3 :: IO Bool
test3 = do
    r <- startPParse getBlock  $ pack [30,0,0,0, 0x56, 49, 50,51,52,53, 5,0, 0x56, 49, 50,51,52,53, 5,0]
    case r of
        Done bs -> return $ bs == pack [49, 50,51,52,53,49, 50,51,52,53,49, 50,51,52,53,
                                        49, 50,51,52,53,49, 50,51,52,53,49, 50,51,52,53]
        _ -> return False



