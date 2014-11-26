-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- AlephCloud.ACN.Event.Encode
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

module AlephCloud.ACN.Event.Encode
    ( toLazyByteString
    , fromLazyByteString
    , toByteString
    , fromByteString
    ) where

import Control.Applicative
import AlephCloud.ACN.Event.Types
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGetIncremental, Decoder(..), isEmpty, pushChunks)
import Data.Binary (put, get)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

toLazyByteString :: [TaggedVal] -> L.ByteString
toLazyByteString vals = runPut (mapM_ put vals)

toByteString :: [TaggedVal] -> B.ByteString
toByteString = L.toStrict . toLazyByteString

fromLazyByteString :: L.ByteString -> Either String [TaggedVal]
fromLazyByteString l = toResult (runGetIncremental loop `pushChunks` l)
  where loop = do
            b <- isEmpty
            if b then return [] else (:) <$> get <*> loop
        toResult (Fail _ _ s) = Left s
        toResult (Partial _)  = Left "partial"
        toResult (Done _ _ a) = Right a

fromByteString :: B.ByteString -> Either String [TaggedVal]
fromByteString = fromLazyByteString . L.fromStrict
