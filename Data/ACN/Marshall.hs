-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Data.ACN.Marshall
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

-- |
-- Module      : Data.ACN.Marshall
-- Copyright: Copyright (c) 2013-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer  : licensing@pivotmail.com
--

{-# LANGUAGE OverloadedStrings #-}

module Data.ACN.Marshall
    ( toTaggedVal, fromTaggedVal
    ) where

import Data.ACN.Types
import Data.ACN.Event
import Data.ACN.LEB128
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
toTaggedVal (AcnID (cat,sub)) = TaggedVal 6 (B.singleton cat `B.append` intToBS sub)
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
        6 -> case B.uncons b of
                Nothing     -> Nothing
                Just (c,b') -> Just $ AcnID (c, intFromBS b')
        7 -> Just $ AcnSeqStart
        8 -> Just $ AcnSeqEnd
        9 -> Just $ AcnSetStart
        10 -> Just $ AcnSetEnd
        _ -> Nothing
