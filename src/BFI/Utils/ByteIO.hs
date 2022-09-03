module BFI.Utils.ByteIO
  ( getByte,
    putByte,
    hGetByte,
    hPutByte,
  )
where

import Data.Binary (Word8)
import Data.ByteString (ByteString, hGet, hPutStr, pack, unpack)
import System.IO (Handle, stdin, stdout)

getByte :: IO Word8
getByte = hGetByte stdin

putByte :: Word8 -> IO ()
putByte = hPutByte stdout

hGetByte :: Handle -> IO Word8
hGetByte hdl = unpackByte <$> hGet hdl 1

hPutByte :: Handle -> Word8 -> IO ()
hPutByte hdl b = hPutStr hdl $ packByte b

--

unpackByte :: ByteString -> Word8
unpackByte = head . unpack

packByte :: Word8 -> ByteString
packByte b = pack [b]
