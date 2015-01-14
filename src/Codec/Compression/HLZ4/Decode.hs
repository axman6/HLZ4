{-# LANGUAGE BangPatterns, ExistentialQuantification,
    TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    RecordWildCards #-}

module Codec.Compression.HLZ4.Decode
    -- (
    -- decompress
    -- )
    where


import Control.Monad (when)
import Control.Monad.IO.Class

import Control.Applicative ((<$>), Applicative(..))

import Data.Bits ((.|.), (.&.), shiftR, shiftL, testBit, clearBit)

import Data.ByteString as BS (ByteString, pack, drop, length)
import Data.ByteString.Internal (toForeignPtr, fromForeignPtr)

import Data.Word (Word8, Word16, Word32)

import Foreign.ForeignPtr (touchForeignPtr, newForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
import Foreign.Marshal.Utils as F (copyBytes)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Foreign.Storable (peekByteOff, peek, poke)

import Text.Printf

debug :: Bool
debug = False

type P8 = Ptr Word8
-- | tuple of src and dest pointers, offset into and their lengths
-- type PtrState = (P8, Int, Int, P8, Int, Int)
data PtrState = PS {
    _src  :: !P8,
    _soff :: !Int,
    _slen :: !Int,
    _dst  :: !P8,
    _doff :: !Int,
    _dlen :: !Int}

advanceSrcState :: Int -> PtrState -> PtrState
advanceSrcState n st =
    st {_soff = _soff st + n}

advanceDestState :: Int -> PtrState -> PtrState
advanceDestState n st =
    st {_doff = _doff st + n}


advanceBothState :: Int -> PtrState -> PtrState
advanceBothState n st =
    st {_soff = _soff st + n, _doff = _doff st + n}



newtype Decoder a = Decoder (PtrState -> IO (PtrState, DResult a))

runDecoder :: Decoder a -> PtrState -> IO (PtrState, DResult a)
runDecoder (Decoder p) = p

data DResult a
    = DDone a
            -- ^ returns the src ptr and the length remaining, and a pointer to
    | DPartial (Decoder a)
        -- ^ More input is required, pass in a Ptr Word8 and the length in bytes
    | DError String
        -- ^ Something bad happened

instance Show a => Show (DResult a) where
    show x = case x of
        DDone a -> "DDone " ++ show a
        DError str -> "DError: " ++ str
        DPartial _ -> "DPartial <p>"


instance Functor Decoder where
    fmap f m = m >>= return . f

instance Applicative Decoder where
    pure = return
    mf <*> ma = mf >>= \f -> ma >>= \a -> return (f a)

instance Monad Decoder where
    return a = Decoder $ \s -> return (s,DDone a)
    {-# INLINE return #-}
    Decoder m >>= f = Decoder $ \s -> do
        (s',r) <- m s
        case r of
            DDone x   ->
                let Decoder fx = f x in fx s'
            DPartial g  ->
                return $ (s',DPartial (g >>= f))
            DError str  ->
                return (s',DError str)
    {-# INLINE (>>=) #-}



instance MonadIO Decoder where
    liftIO m = Decoder $ \s -> do
        x <- m
        return (s,DDone x)

-- setSrc :: (P8,Int,Int) -> Decoder ()
-- setSrc (src,soff,slen) = do
--     Decoder $ \(_,_,_,dst,doff,dlen) ->
--         return ((src,soff,slen,dst,doff,dlen),DDone ())


demandInput :: Decoder ()
demandInput = Decoder $ \s -> return (s,DPartial (return ()))

getState :: Decoder PtrState
getState = Decoder $ \s -> return (s, DDone s)

putState :: PtrState -> Decoder ()
putState s = Decoder $ \_s -> return (s,DDone ())

modifyState :: (PtrState -> PtrState) -> Decoder ()
modifyState f = Decoder $ \s -> return (f s,DDone ())


startDecoder :: Show a => Decoder a -> ByteString -> IO HLZ4Result
startDecoder p bs = do
   let (srcptr, off, len) = toForeignPtr bs
   r <- runD bs (PS (unsafeForeignPtrToPtr srcptr) off len nullPtr 0 0) p
   touchForeignPtr srcptr
   return r



runD :: Show a => ByteString -> PtrState -> Decoder a -> IO HLZ4Result
runD input s (Decoder p) = do
    (s',x) <- p s
    when debug $ print x
    case x of
        DDone _  -> do
            fptr <- newForeignPtr finalizerFree (_dst s')
            let res = (fromForeignPtr fptr 0 (_doff s'))
            if (_soff s' == _slen s')
                then return $ Done res
                else return $ Block res (BS.drop (_soff s') input)

        DPartial g ->
            return $ Partial (feed s' g)
        DError str -> return $ Error str


feed :: Show a => PtrState -> Decoder a -> ByteString -> IO HLZ4Result
feed st g bs = do
    let (srcptr, off, len) = toForeignPtr bs
    r <- runD bs (st {_src = unsafeForeignPtrToPtr srcptr, _soff = off, _slen = len}) g
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



err :: String -> Decoder a
err str = Decoder $ \s -> return $ (s,DError str)



getSrcRemaining :: Decoder Int
getSrcRemaining = getState >>= \st -> return (_slen st - _soff st)

getDestRemaining :: Decoder Int
getDestRemaining = getState >>= \st -> return (_dlen st - _doff st)




advanceSrc :: Int -> Decoder ()
advanceSrc n = modifyState(advanceSrcState n)

advanceDest :: Int -> Decoder ()
advanceDest n = modifyState(advanceDestState n)

advanceBoth :: Int -> Decoder ()
advanceBoth n = modifyState(advanceBothState n)


peekByte :: Ptr Word8 -> Int -> IO Word8
peekByte = peekByteOff

fi8 :: Integral a => Word8 -> a
fi8 = fromIntegral

getWord8 :: Decoder Word8
getWord8 = do
    PS {..} <- getState
    if _soff < _slen
        then do
            advanceSrc 1
            b <- liftIO $ peekByte _src _soff
            when debug $ liftIO $ printf "getWord8: %d\n" b
            return b
        else demandInput >> getWord8
{-# INLINE getWord8 #-}

getWord16LE :: Decoder Word16
getWord16LE = do
    PS {..} <- getState
    if _slen - _soff >= 2
        then do
            advanceSrc 2
            liftIO $ peek (castPtr (_src `plusPtr` _soff))
        else do
            a <- getWord8
            b <- getWord8
            return (fi8 b `shiftL` 8 .|. fi8 a)
{-# INLINE getWord16LE #-}


getWord32LE :: Decoder Word32
getWord32LE = do
    PS {..} <- getState
    if _slen - _soff >= 4
        then do
            advanceSrc 4
            liftIO $ peek (castPtr (_src `plusPtr` _soff))
        else do
            a <- getWord8
            b <- getWord8
            c <- getWord8
            d <- getWord8
            return (fi8 d `shiftL` 24 .|.  fi8 c `shiftL` 16 .|. fi8 b `shiftL` 8 .|. fi8 a)
{-# INLINE getWord32LE #-}


-- | Allocate a new destination buffer, returning the bytestring representing the
-- current destination buffer.
allocateDest :: Int -> Decoder ByteString
allocateDest n =
    Decoder $ \st -> do
        fptr <- newForeignPtr finalizerFree (_dst st)
        let res = fromForeignPtr fptr 0 (_doff st)
        dst' <- mallocBytes n
        return ( st {_dst = dst', _doff = 0, _dlen = n}, DDone res)


-- | Decodes a number encoded using repeated values of 255 and returns how many bytes were used
getLength :: Decoder Int
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
transfer :: Int -> Decoder ()
transfer count = do
    PS {..} <- getState
    if _doff + count >= _dlen
        then err $ "transfer: transfer of "
                 ++ show count
                 ++ " bytes would overflow destination buffer "
                 ++ show (_doff,_dlen)
        else if _soff + count < _slen
            then do
                liftIO $ F.copyBytes (_dst `plusPtr` _doff) (_src `plusPtr` _soff) count
                advanceBoth count
            else do
                let srem = _slen - _soff
                liftIO $ F.copyBytes (_dst `plusPtr` _doff) (_src `plusPtr` _soff) srem
                advanceDest srem
                demandInput
                transfer (count-srem)


-- | Moves `count` bytes of data from `offset` bytes before current position into
-- the current position in the destination, and advances the state. If count is greater
-- than the space remaining in the destination buffer, an error is returned.
lookback :: Int -> Int -> Decoder ()
lookback count offset = do
    PS {..} <- getState
    when (_doff + count > _dlen) $
        err $ "lookback: copy of " ++ show count ++ " bytes would overflow destination buffer"
    when (offset > _doff) $
        err $ "lookback: copy from offset " ++ show offset ++ " before beginning of buffer, dest offset: " ++ show _doff
    let mmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
        mmove !_ !_ 0 = return ()
        mmove dest src n = peek src >>= poke dest
                           >> mmove (dest `plusPtr` 1) (src `plusPtr` 1) (n-1)

    liftIO $ mmove (_dst `plusPtr` _doff) (_dst `plusPtr` (_doff-offset)) count
    advanceDest count


getByteString :: Int -> Decoder ByteString
getByteString len = do
    bsptr <- liftIO $ mallocBytes len
    go bsptr len
    fptr <- liftIO $ newForeignPtr finalizerFree bsptr
    return $ fromForeignPtr fptr 0 len

    where go ptr len = do
            PS {..} <- getState
            if _soff + len < _slen
                then do
                    liftIO $ F.copyBytes ptr (_src `plusPtr` _soff) len
                    advanceSrc len
                else do
                    let srem = _slen-_soff
                    liftIO $ F.copyBytes ptr (_src `plusPtr` _soff) srem
                    demandInput
                    go (ptr `plusPtr` srem) (len-srem)

-- | Decodes a single LZ4 sequence within a block (lit len, lits, offset backwards, copy len).
-- Returns the number of bytes written to the destination
decodeSequence :: Decoder Int
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

decodeSequences :: Int -> Decoder ()
decodeSequences len
    | len < 0   = err $ "decodeSequence: read more than block length bytes from source: " ++ show len
    | len == 0  = return ()
    | otherwise = do
        ssize <- decodeSequence
        decodeSequences (len-ssize)

getBlock :: Decoder ()
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
    r <- startDecoder getBlock $ pack [15,0,0,0, 0x56, 49,50,51,52,53, 5,0]
    case r of
        Done bs -> return $ bs == pack [49, 50,51,52,53,49,50,51,52,53,49, 50,51,52,53]
        _ -> return False


test2 :: IO Bool
test2 = do
    r <- startDecoder getBlock $ pack [15,0,0,0, 0x56, 49,50,51,52,53, 5,0,0,0,0,0]
    case r of
        Block bs res -> return $ bs  == pack [49, 50,51,52,53,49,50,51,52,53,49, 50,51,52,53]
                              && res == pack [0,0,0,0]
        _ -> return False

test3 :: IO Bool
test3 = do
    r <- startDecoder getBlock $ pack [30,0,0,0, 0x56, 49,50,51,52,53, 5,0, 0x56, 49, 50,51,52,53, 5,0]
    case r of
        Done bs -> return $ bs == pack [49,50,51,52,53,49,50,51,52,53,49,50,51,52,53,
                                        49,50,51,52,53,49,50,51,52,53,49,50,51,52,53]
        _ -> return False



