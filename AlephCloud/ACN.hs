-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- AlephCloud.ACN
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

module AlephCloud.ACN
    ( module AlephCloud.ACN.Types
    , module AlephCloud.ACN.Event
    , module AlephCloud.ACN.Marshall
    , acnObjectFromBytes
    , acnObjectToBytes
    , acnStreamToBytes
    , acnStreamFromBytes
    ) where

import AlephCloud.ACN.Types
import AlephCloud.ACN.Event
import AlephCloud.ACN.Marshall
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

acnObjectFromBytes :: FromACN a => B.ByteString -> Either String (a, [Acn])
acnObjectFromBytes =
    either (Left . snd) (maybe (Left "Invalid tagged val") fromACN . sequence . map fromTaggedVal) . runParse . L.fromStrict

acnObjectToBytes :: ToACN a => a -> B.ByteString
acnObjectToBytes = acnStreamToBytes . toACN

acnStreamToBytes :: [Acn] -> B.ByteString
acnStreamToBytes as = toByteString $ map toTaggedVal as

acnStreamFromBytes :: B.ByteString -> Either String [Acn]
acnStreamFromBytes =
    either (Left . snd) (sequence . map fromTaggedValE) . runParse . L.fromStrict
  where fromTaggedValE t = maybe (Left $ "unknown or invalid ACN object: " ++ show t) Right $ fromTaggedVal t
