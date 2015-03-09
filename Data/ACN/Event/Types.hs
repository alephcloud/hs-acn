-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Data.ACN.Event.Types
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

module Data.ACN.Event.Types
    ( TaggedVal(..)
    ) where

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary (Binary(..))

import Data.ACN.LEB128

-- | A tagged value
data TaggedVal = TaggedVal
    { getTaggedTag   :: Word8
    , getTaggedValue :: ByteString
    } deriving (Show,Eq)

instance Binary TaggedVal where
    put (TaggedVal t v) =
        putWord8 t >> putULEB128 (fromIntegral $ B.length v) >> putByteString v
    get = TaggedVal <$> getWord8 <*> (getULEB128 >>= getByteString . fromIntegral)
