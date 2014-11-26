-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- AlephCloud.ACN.Types
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
{-# LANGUAGE FlexibleInstances #-}
module AlephCloud.ACN.Types where

import Data.ByteString (ByteString)
import Data.Word

type ID = (Word8, Integer)
type BitArray = ByteString

-- | Abstract Syntax Notation
data Acn =
      AcnBytes ByteString
    | AcnString String
    | AcnBool Bool
    | AcnInt Integer
    | AcnID ID
    | AcnSeqStart -- ^ start a sequence/record
    | AcnSeqEnd   -- ^ end a sequence/record
    | AcnSetStart -- ^ start a set/array
    | AcnSetEnd   -- ^ end a set/array
    deriving (Show,Eq)

class ToACN a where
    toACN :: a -> [Acn]
class FromACN a where -- not sure if String is the best "error" type..
    fromACN :: [Acn] -> Either String (a, [Acn])

instance ToACN ByteString where
    toACN = (:[]) . AcnBytes
instance FromACN ByteString where
    fromACN (AcnBytes bs:r) = Right (bs, r)
    fromACN _               = Left "expecting AcnBytes"
instance ToACN Bool where
    toACN = (:[]) . AcnBool
instance FromACN Bool where
    fromACN (AcnBool b:r) = Right (b, r)
    fromACN _             = Left "expecting AcnBool"
instance ToACN String where
    toACN = (:[]) . AcnString
instance FromACN String where
    fromACN (AcnString s:r) = Right (s, r)
    fromACN _               = Left "expecting AcnString"

instance ToACN Integer where
    toACN i = [AcnInt i]
instance FromACN Integer where
    fromACN (AcnInt i:r) = Right (i,r)
    fromACN _            = Left "expecting AcnInt"

instance ToACN Int where
    toACN i = [AcnInt (fromIntegral i)]
instance FromACN Int where
    fromACN (AcnInt i:r) = Right (fromIntegral i,r)
    fromACN _            = Left "expecting AcnInt"

instance ToACN a => ToACN [a] where
    toACN l = [AcnSetStart] ++ concatMap toACN l ++ [AcnSetEnd]
instance FromACN a => FromACN [a] where
    fromACN (AcnSetStart:after) = loop after
      where loop (AcnSetEnd:r) = Right ([], r)
            loop [] = Left "expecting AcnSetEnd"
            loop l = case fromACN l of
                        Left err     -> Left err
                        Right (a, r) -> case loop r of
                                            Right (vals, r') -> Right (a : vals, r')
                                            Left err         -> Left err
    fromACN _ = Left "expecting AcnSetStart"

instance ToACN a => ToACN (Maybe a) where
    toACN Nothing = [AcnSeqStart,AcnSeqEnd]
    toACN (Just a) = [AcnSeqStart] ++ toACN a ++ [AcnSeqEnd]
instance FromACN a => FromACN (Maybe a) where
    fromACN (AcnSeqStart:AcnSeqEnd:l) = Right (Nothing, l)
    fromACN (AcnSeqStart:l) =
        case fromACN l of
            Left err                  -> Left ("FromACN Maybe: inner parsing failed: " ++ err)
            Right (o, (AcnSeqEnd:l')) -> Right (Just o, l')
            Right (_, _)              -> Left ("FromACN Maybe: unparsed data in maybe container")
    fromACN _ = Left "FromACN Maybe: Expecting AcnSeqStart"

instance (ToACN a1, ToACN a2) => ToACN (a1,a2) where
    toACN (a1,a2) = toACN a1 ++ toACN a2
instance (FromACN a1, FromACN a2) => FromACN (a1,a2) where
    fromACN r = do
        (a1, r1) <- fromACN r
        (a2, r') <- fromACN r1
        return ((a1,a2), r')
instance (ToACN a1, ToACN a2, ToACN a3) => ToACN (a1,a2,a3) where
    toACN (a1,a2,a3) = concat[toACN a1,toACN a2,toACN a3]
instance (FromACN a1, FromACN a2, FromACN a3) => FromACN (a1,a2,a3) where
    fromACN r = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r') <- fromACN r2
        return ((a1,a2,a3), r')
instance (ToACN a1, ToACN a2, ToACN a3, ToACN a4) => ToACN (a1,a2,a3,a4) where
    toACN (a1,a2,a3,a4) = concat[toACN a1,toACN a2,toACN a3,toACN a4]
instance (FromACN a1, FromACN a2, FromACN a3, FromACN a4) => FromACN (a1,a2,a3,a4) where
    fromACN r = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r3) <- fromACN r2
        (a4, r') <- fromACN r3
        return ((a1,a2,a3,a4), r')
instance (ToACN a1, ToACN a2, ToACN a3, ToACN a4, ToACN a5) => ToACN (a1,a2,a3,a4,a5) where
    toACN (a1,a2,a3,a4,a5) = concat[toACN a1,toACN a2,toACN a3,toACN a4,toACN a5]
instance (FromACN a1, FromACN a2, FromACN a3, FromACN a4, FromACN a5) => FromACN (a1,a2,a3,a4,a5) where
    fromACN r = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r3) <- fromACN r2
        (a4, r4) <- fromACN r3
        (a5, r') <- fromACN r4
        return ((a1,a2,a3,a4,a5), r')
instance (ToACN a1, ToACN a2, ToACN a3, ToACN a4, ToACN a5, ToACN a6) => ToACN (a1,a2,a3,a4,a5,a6) where
    toACN (a1,a2,a3,a4,a5,a6) = concat[toACN a1,toACN a2,toACN a3,toACN a4,toACN a5,toACN a6]
instance (FromACN a1, FromACN a2, FromACN a3, FromACN a4, FromACN a5, FromACN a6) => FromACN (a1,a2,a3,a4,a5,a6) where
    fromACN r = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r3) <- fromACN r2
        (a4, r4) <- fromACN r3
        (a5, r5) <- fromACN r4
        (a6, r') <- fromACN r5
        return ((a1,a2,a3,a4,a5,a6), r')
