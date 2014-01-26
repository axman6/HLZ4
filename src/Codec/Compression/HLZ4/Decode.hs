{-# LANGUAGE BangPatterns, ExistentialQuantification, 
    TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Codec.Compression.HLZ4.Decode
    -- (
    -- decompress
    -- ) 
    where


import Control.Monad (when)
import Control.Monad.IO.Class

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
type PtrState = (P8, P8, Int, Int)

advanceSrcState :: Int -> PtrState -> PtrState
advanceSrcState n (src,dst,srem,drem) =
    (src `plusPtr` n, dst, srem - n, drem)

advanceDestState :: Int -> PtrState -> PtrState
advanceDestState n (src,dst,srem,drem) =
    (src, dst `plusPtr` n, srem, drem - n)


advanceBothState :: Int -> PtrState -> PtrState
advanceBothState n (src,dst,srem,drem) =
    (src `plusPtr` n, dst `plusPtr` n, srem - n, drem - n)



newtype PtrParser a = PtrParser (PtrState -> IO (PtrState, PPResult a))

runPtrParser :: PtrParser a -> PtrState -> IO (PtrState, PPResult a)
runPtrParser (PtrParser p) = p

data PPResult a
    = PPDone a
            -- ^ returns the src ptr and the length remaining, and a pointer to 
    | PPPartial (PtrParser a)
        -- ^ More input is required, pass in a Ptr Word8 and the length in bytes
    | PPSetDest Int (PtrParser a)
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
    
setSrc :: (P8,Int) -> PtrParser ()
setSrc (src,srem) = do 
    PtrParser $ \(_,dst,_,drem) ->
        return ((src,dst,srem,drem),PPDone ())


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
   let (srcptr, off, srem) = toForeignPtr bs
   r <- runPP bs (nullPtr,0) ((unsafeForeignPtrToPtr srcptr `plusPtr` off),nullPtr,srem,0) p
   touchForeignPtr srcptr
   return r



runPP :: Show a => ByteString -> (P8,Int) -> PtrState -> PtrParser a -> IO HLZ4Result
runPP input r s (PtrParser p) = do
    (s'@(src,_,srem,_),x) <- p s
    when debug $ print x
    case x of
        PPDone _  -> do
            let (dst,len) = r
            fptr <- newForeignPtr finalizerFree dst
            let res = (fromForeignPtr fptr 0 len)
            if (srem == 0)
                then return $ Done res
                else return $ Block res (BS.drop (BS.length input - srem) input)
                    
        PPPartial g -> 
            return $ Partial (feed r s' g)
        PPSetDest len' p' -> do
            dst' <- mallocBytes len'
            runPP input (dst',len') (src,dst',srem,len') p'
        PPError str -> return $ Error str


feed :: Show a => (P8,Int) -> PtrState -> PtrParser a -> ByteString -> IO HLZ4Result
feed res (_,dst,_,drem) g bs = do
    let (srcptr, off, srem) = toForeignPtr bs
    r <- runPP bs res ((unsafeForeignPtrToPtr srcptr `plusPtr` off),dst,srem,drem) g
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
getSrcRemaining = getState >>= \(_,_,srem,_) -> return srem

getDestRemaining :: PtrParser Int
getDestRemaining = getState >>= \(_,_,_,drem) -> return drem




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
    (src,_,srem,_) <- getState
    if srem >= 1
        then do
            b <- liftIO $ peekByte src 0
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
    (src,dst,srem,drem) <- getState
    case () of
        _ | count <= srem && count <= drem -> do
                liftIO $ F.copyBytes dst src count
                advanceBoth count
          | count > srem && count <= drem -> do
                liftIO $ F.copyBytes dst src srem
                advanceDest srem
                demandInput
                transfer (count-srem)
          | otherwise -> err $ "transfer: transfer of " ++ show count ++ " bytes would overflow destination buffer"

-- 
-- | Moves `count` bytes of data from `offset` bytes before current position into
-- the current position in the destination, and advances the state. Unchecked.
lookback :: Int -> Int -> PtrParser ()
lookback count offset = do
    (_,dst,_,drem) <- getState
    when (count > drem) $ err $ "lookback: copy of " ++ show count ++ " bytes would overflow destination buffer"
    let mmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
        mmove !_ !_ 0 = return ()
        mmove dest src n = peek src >>= poke dest >> mmove (dest `plusPtr` 1) (src `plusPtr` 1) (n-1)
    
    liftIO $ mmove dst (dst `plusPtr` (-offset)) count
    advanceDest count

-- 
-- 
getSrcProgress :: Int -> PtrParser Int
getSrcProgress n = do
    srem <- getSrcRemaining
    return $ n-srem

getDestProgress :: Int -> PtrParser Int
getDestProgress n = do
    drem <- getDestRemaining
    return $ n-drem


-- | Decodes a single LZ4 sequence within a block (lit len, lits, offset backwards, copy len).
-- Returns the number of bytes 
decodeSequence :: PtrParser Int
decodeSequence = do
    sremBefore <- getDestRemaining
    token <- getWord8
    let lLen = fromIntegral $ (token .&. 0xF0) `shiftR` 4
        mLen = fromIntegral $ (token .&. 0x0F) + 4
    -- Write literals
    litLength <- if lLen == 15 
                then (15+) `fmap` getLength 
                else return lLen
    transfer   litLength
    
    -- copy length from offset
    drem <- getDestRemaining
    when (drem > 0) $ do
        offset <- getWord16LE
        matchLen <- if mLen == 19
            then (19+) `fmap` getLength
            else return mLen
        lookback matchLen (fromIntegral offset)
    
    getDestProgress sremBefore

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



