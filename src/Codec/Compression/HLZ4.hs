

import Data.Bytestring ()
import Data.Bytestring.Internal (toForeignPtr)




data HLZ4Result
    = Done ByteString --^ Finished decoding all there is
    | Block ByteString ByteString --^ Decoded a block, and data remains
    | Partial (ByteString -> HLZ4Result) --^ More data is needed to decompress
    | Error String --^ Something bad happened


decompress :: ByteString -> Maybe ByteString
decompress = undefined

decodeBlock :: ByteString -> HLZ4Result
decodeBlock bs =
    let (fptr, off, len) toForeignPtr bs