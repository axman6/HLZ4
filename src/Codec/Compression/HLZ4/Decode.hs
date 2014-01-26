{-# LANGUAGE BangPatterns, ExistentialQuantification, 
    TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Codec.Compression.HLZ4.Decode
    -- (
    -- decompress
    -- ) 
    where


import Control.Monad (when)
import Control.Monad.Cont
import Control.Monad.State.Strict

import Data.Bits ((.|.), (.&.), shiftR, testBit, clearBit)

import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr)

import Data.Word (Word8, Word16, Word32)

import Foreign.ForeignPtr (unsafeForeignPtrToPtr)

import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils as F (copyBytes)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (peekByteOff, peek, poke)




data HLZ4Result
    = Done ByteString -- ^ Finished decoding all there is
    | Block ByteString ByteString -- ^ Decoded a block, and remiaing input
    | Partial (ByteString -> IO HLZ4Result) -- ^ More data is needed to decompress
    | Error String -- ^ Something bad happened

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


-- type PtrParser a = ContT () (StateT PtrState IO) (PPResult a)


-- instance Monad PPResult where
--     return a = PPReturn a
--     PPReturn a >>= f = f a
--     PPDstPtr ptr len p >>= f = PPDstPtr ptr len (p >>= f)
--     PPPartial p >>= f = PPPartial (p >>= f)
--     d@(PPDone _ _) >>= _ = d
--     PPError str >>= _ = PPError str



-- type PtrParser r = forall a. (PtrState -> (a -> IO (PtrState,PPResult r)) -> IO (PtrState, PPResult r))


newtype PtrParser a = PtrParser (PtrState -> IO (PtrState, PPResult a))

runPrtParser :: PtrParser a -> PtrState -> IO (PtrState, PPResult a)
runPrtParser (PtrParser p) = p

data PPResult a
    = PPReturn a
    | PPDone P8 Int
        -- ^ returns the src ptr and the length remaining, and a pointer to 
    | PPDstPtr P8 Int (PtrParser a) 
        -- ^ when a new dest pointer is allocated, returns it and its length and the
        -- parser to be run means that the parser doesn't have to keep track of
        -- the beginning of the destination, runPtrParser can instead
    | PPPartial ((P8,Int) -> PtrParser a)
        -- ^ More input is required, pass in a Ptr Word8 and the length in bytes
    | PPError String
        -- ^ Something bad happened
    -- | Value a -- not sure if needed


instance Monad PtrParser where
    return a = PtrParser $ \s -> return (s,PPReturn a)
    PtrParser m >>= f = PtrParser $ \s -> do
        (s',r) <- m s
        case r of
            PPReturn x -> let PtrParser fx = f x in fx s'
            PPPartial g -> PPPartial (\s'' -> g s'' >>= f)

setSrc :: (P8,Int) -> PtrParser a -> PtrParser a
setSrc (src,srem) = PtrParser $ 

-- runPP :: P8 -> Int -> PtrParser a -> IO HLZ4Result
-- runPP src srem p = do
--     let stT = runContT p id
--     (res,(src',srem',dst',drem')) <- runStateT stT (src,nullPtr,srem,0)
--     case res of
--         PPPartial p' -> Partial (feedByteString dst' drem' p')
--         
-- 
-- 
-- 
-- 
-- feedByteString :: P8 -> Int -> PtrParser a -> ByteString -> IO HLZ4Result
-- feedByteString dst drem p bs = do
--     let (srcptr, off, srem) = toForeignPtr bs
--     runPP (unsafeForeignPtrToPtr srcptr `plusPtr` off) srem p






-- err :: String -> PtrParser a
-- err str = ContT $ \_ -> return $ PPError str



-- getSrcRemaining :: PtrParser Int
-- getSrcRemaining = get >>= \(_,_,srem,_) -> return srem
-- 
-- getDestRemaining :: PtrParser Int
-- getDestRemaining = get >>= \(_,_,_,drem) -> return drem
-- 
-- 
-- 
-- 
-- advanceSrc :: Int -> PtrParser ()
-- advanceSrc n = modify (advanceSrcState n)
-- 
-- advanceDest :: Int -> PtrParser ()
-- advanceDest n = modify (advanceDestState n)
-- 
-- advanceBoth :: Int -> PtrParser ()
-- advanceBoth n = modify (advanceBothState n)
-- 
-- 
-- peekByte :: Ptr Word8 -> Int -> IO Word8
-- peekByte = peekByteOff

-- demandInput :: PtrParser a -> PtrParser a
-- demandInput p = ContT $ \next -> 
--     return $ PPPartial (p >>= \x -> ContT $ \c -> (next x >>= c))
-- 
-- getWord8 :: PtrParser Word8
-- getWord8 = do
--     (src,_,srem,_) <- get
--     if srem >= 1
--         then do
--             b <- liftIO $ peekByte src 0
--             advanceSrc 1
--             return b
--         else demandInput getWord8 
-- {-# INLINE getWord8 #-}
-- 
-- getWord16LE :: PtrParser Word16
-- getWord16LE = do
--     a' <- getWord8
--     b' <- getWord8
--     let [a,b] = map fromIntegral [a',b']
--     return (a `shiftR` 8 .|. b)
-- {-# INLINE getWord16LE #-}
-- 
-- 
-- getWord32LE :: PtrParser Word32
-- getWord32LE = do
--     a' <- getWord8
--     b' <- getWord8
--     c' <- getWord8
--     d' <- getWord8
--     let [a,b,c,d] = map fromIntegral [a',b',c',d']
--     return (a `shiftR` 24 .|. b `shiftR` 16 .|. c `shiftR` 8 .|. d)
-- {-# INLINE getWord32LE #-}
-- 
-- 
-- setDestination :: Int -> PtrParser ()
-- setDestination n = do
--     dst <- liftIO $ mallocBytes n
--     (src, _, srem, _) <- get
--     put (src, dst, srem, n)
--     ContT $ \next -> do
--         let f :: (() -> StateT PtrState IO PPResult) -> ()
--             f _ = ()
--         return (f next)
--         return $ PPDstPtr dst n (ContT $ \c -> (next () >>= c))
-- 
-- -- | Decodes a number encoded using repeated values of 255 and returns how many bytes were used
-- getLength :: PtrParser Int
-- getLength = go 0 where
--     go n = do
--         b <- getWord8
--         case b of
--             255 -> go (n+255)
--             _   -> return (n + fromIntegral b)   
-- {-# INLINE getLength #-}
--  
-- -- | Transfers `count` bytes from src into dest. If there are not enough
-- -- bytes in src, the remaining bytes will be copied and more input demanded.
-- -- If there is not enough room in the destination and error is produced.
-- transfer :: Int -> PtrParser ()
-- transfer count = do
--     (src,dst,srem,drem) <- get
--     case () of
--         _ | count <= srem && count <= drem -> do
--                 liftIO $ F.copyBytes dst src count
--                 advanceBoth count
--           | count > srem && count <= drem -> do
--                 liftIO $ F.copyBytes dst src srem
--                 advanceDest srem
--                 demandInput (transfer (count-srem))
--           | otherwise -> err "transfer: requested too many bytes, destination overflow"
-- 
-- -- 
-- -- | Moves `count` bytes of data from `offset` bytes before current position into
-- -- the current position in the destination, and advances the state. Unchecked.
-- lookback :: Int -> Int -> PtrParser ()
-- lookback count offset = do
--     (_,dst,_,drem) <- get
--     when (count > drem) $ err "lookback: requested too many bytes, destination overflow"
--     let mmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
--         mmove dest src 0 = return ()
--         mmove dest src n = peek src >>= poke dest >> mmove (dest `plusPtr` 1) (src `plusPtr` 1) (n-1)
--     
--     liftIO $ mmove dst (dst `plusPtr` (-offset)) count
--     advanceDest count
-- 
-- -- 
-- -- 
-- getSrcProgress :: Int -> PtrParser Int
-- getSrcProgress n = do
--     srem <- getSrcRemaining
--     return $ n-srem
-- 
-- getDestProgress :: Int -> PtrParser Int
-- getDestProgress n = do
--     drem <- getDestRemaining
--     return $ n-drem
-- 
-- 
-- -- | Decodes a single LZ4 sequence within a block (lit len, lits, offset backwards, copy len).
-- -- Returns the number of bytes 
-- decodeSequence :: PtrParser Int
-- decodeSequence = do
--     sremBefore <- getSrcRemaining
--     token <- getWord8
--     let lLen = fromIntegral $ (token .&. 0xF0) `shiftR` 4
--         mLen = fromIntegral $ (token .&. 0x0F) + 4
--     -- Write literals
--     litLength <- if lLen == 15 
--                 then (15+) `fmap` getLength 
--                 else return lLen
--     transfer   litLength
--     
--     -- copy length from offset
--     drem <- getDestRemaining
--     when (drem > 0) $ do
--         offset <- getWord16LE
--         matchLen <- if mLen == 19
--             then (19+) `fmap` getLength
--             else return mLen
--         lookback matchLen (fromIntegral offset)
--     
--     getSrcProgress sremBefore
-- 
-- decodeSequences :: Int -> PtrParser ()
-- decodeSequences len 
--     | len < 0   = fail "decodeSequence: read more than block length bytes from source"
--     | len == 0  = return ()
--     | otherwise = do
--         ssize <- decodeSequence
--         decodeSequences (len-ssize)
--     
-- getBlock :: PtrParser ()
-- getBlock = do
--     len' <- getWord32LE
--     let len = fromIntegral (len' `clearBit` 31)
--     setDestination len
--     if testBit len' 31
--         then transfer len
--         else decodeSequences len
--         
--     
-- 
-- 
-- 
