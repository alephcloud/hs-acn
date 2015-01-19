-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Data.ACN.Event.Parse
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

module Data.ACN.Event.Parse
    ( TaggedVal(..)
    , runParse
    ) where

import Control.Monad (liftM2)
import Data.Word
import Data.Binary (get)
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L

import Data.ACN.Event.Types

-- | run parser on a lazy bytestring
-- TODO add an incremental interface ..
runParse :: L.ByteString -> Either (Word64, String) [TaggedVal]
runParse = either toErr toVals . runGetOrFail getTLVs
  where toErr (_, pos, err)   = Left (fromIntegral pos, err)
        toVals (left, pos, a)
            | L.null left = Right a
            | otherwise   = Left (fromIntegral pos, "remaining bytes: " ++ (show $ L.take 10 left) ++ "...")
        getTLVs = do
            endStream <- isEmpty
            if endStream
                then return []
                else liftM2 (:) get getTLVs

