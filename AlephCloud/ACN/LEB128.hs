-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- AlephCloud.ACN.LEB128
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
--

module AlephCloud.ACN.LEB128
    ( getULEB128
    , getSLEB128
    , putULEB128
    , putSLEB128
    ) where

import Control.Applicative
import Control.Arrow (second)
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

getLEB128 :: Get (Integer, (Bool, Int))
getLEB128 = loop 0 0
  where loop acc weight = do
            w <- getWord8
            if not (w `testBit` 7)
                then return (acc .|. (fromIntegral w `shiftL` weight), (w `testBit` 6, weight + 7))
                else loop (acc .|. (fromIntegral (w .&. 0x7f) `shiftL` weight)) (weight + 7)


getULEB128 :: Get Integer
getULEB128 = fst <$> getLEB128

getSLEB128 :: Get Integer
getSLEB128 = toSLEB <$> getLEB128
  where toSLEB (n, (True,w)) = n .|. (-(1 `shiftL` w))
        toSLEB (n, _) = n

putULEB128 :: Integer -> Put
putULEB128 n
    | n < 0x80  = putWord8 (fromIntegral n)
    | otherwise = loop n
  where loop v =
            let (d,m) = divMod128 v
             in if d == 0
                    then putWord8 m
                    else putWord8 (m .|. 0x80) >> loop d
        divMod128 :: Integer -> (Integer, Word8)
        divMod128 i = second fromIntegral (i `divMod` 128)

putSLEB128 :: Integer -> Put
putSLEB128 n = loop n
  where isSignBitSet = flip testBit 6
        isSignBitClear = not . isSignBitSet
        loop i =
            let byte = fromIntegral i .&. 0x7f
                i'   = i `shiftR` 7
             in if (i' == 0 && isSignBitClear byte) || (i' == -1 && isSignBitSet byte)
                    then putWord8 byte
                    else putWord8 (0x80 .|. byte) >> loop i'
