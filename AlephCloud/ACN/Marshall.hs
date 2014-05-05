{-# LANGUAGE OverloadedStrings #-}
module AlephCloud.ACN.Marshall
    ( toTaggedVal, fromTaggedVal
    ) where

import AlephCloud.ACN.Types
import AlephCloud.ACN.Event
import AlephCloud.ACN.LEB128
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)

import qualified Data.ByteString.UTF8 as UTF8

intFromBS :: B.ByteString -> Integer
intFromBS = runGet getSLEB128 . L.fromChunks . (:[]) -- B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0

intToBS :: Integer -> B.ByteString
intToBS x = B.concat $ L.toChunks $ runPut $ putSLEB128 x
{-B.pack $ reverse (list x)
  where list i | i <= 0xff = [fromIntegral i]
               | otherwise = (fromIntegral i .&. 0xff) : list (i `shiftR` 8)-}

toTaggedVal :: Acn -> TaggedVal
toTaggedVal (AcnBytes bs) = TaggedVal 1 bs
toTaggedVal (AcnString s) = TaggedVal 2 $ UTF8.fromString s
toTaggedVal (AcnBool b)   = TaggedVal 3 $ B.singleton (if b then 1 else 0)
toTaggedVal (AcnInt i)    = TaggedVal 5 $ intToBS i
toTaggedVal (AcnID oid)   = TaggedVal 6 B.empty
toTaggedVal AcnSeqStart   = TaggedVal 7 B.empty
toTaggedVal AcnSeqEnd     = TaggedVal 8 B.empty
toTaggedVal AcnSetStart   = TaggedVal 9 B.empty
toTaggedVal AcnSetEnd     = TaggedVal 10 B.empty

fromTaggedVal :: TaggedVal -> Maybe Acn
fromTaggedVal (TaggedVal t b) =
    case t of
        1 -> Just $ AcnBytes b
        2 -> Just $ AcnString $ UTF8.toString b
        3 | b == "\x01" -> Just $ AcnBool True
          | b == "\x00" -> Just $ AcnBool False
          | otherwise   -> Nothing
        5 -> Just $ AcnInt (intFromBS b)
        6 -> Just $ AcnID [] -- FIXME
        7 -> Just $ AcnSeqStart
        8 -> Just $ AcnSeqEnd
        9 -> Just $ AcnSetStart
        10 -> Just $ AcnSetEnd
        _ -> Nothing
