{-# LANGUAGE FlexibleInstances #-}
module AlephCloud.ACN.Types where

import Data.ByteString (ByteString)

type ID = [Integer]
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

instance (ToACN a1, ToACN a2) => ToACN (a1,a2) where
    toACN (a1,a2) = [AcnSeqStart] ++ toACN a1 ++ toACN a2 ++ [AcnSeqEnd]
instance (FromACN a1, FromACN a2) => FromACN (a1,a2) where
    fromACN (AcnSeqStart:r) = do
        (a1, r1) <- fromACN r
        (a2, r') <- fromACN r1
        case r' of
            AcnSeqEnd:l -> return ((a1,a2), l)
            _           -> Left "expecting AcnSeqEnd"
instance (ToACN a1, ToACN a2, ToACN a3) => ToACN (a1,a2,a3) where
    toACN (a1,a2,a3) = [AcnSeqStart] ++ toACN a1 ++ toACN a2 ++ toACN a3 ++ [AcnSeqEnd]
instance (FromACN a1, FromACN a2, FromACN a3) => FromACN (a1,a2,a3) where
    fromACN (AcnSeqStart:r) = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r') <- fromACN r2
        case r' of
            AcnSeqEnd:l -> return ((a1,a2,a3), l)
            _           -> Left "expecting AcnSeqEnd"
instance (ToACN a1, ToACN a2, ToACN a3, ToACN a4) => ToACN (a1,a2,a3,a4) where
    toACN (a1,a2,a3,a4) = [AcnSeqStart] ++ toACN a1 ++ toACN a2 ++ toACN a3 ++ toACN a4 ++ [AcnSeqEnd]
instance (FromACN a1, FromACN a2, FromACN a3, FromACN a4) => FromACN (a1,a2,a3,a4) where
    fromACN (AcnSeqStart:r) = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r3) <- fromACN r2
        (a4, r') <- fromACN r3
        case r' of
            AcnSeqEnd:l -> return ((a1,a2,a3,a4), l)
            _           -> Left "expecting AcnSeqEnd"
instance (ToACN a1, ToACN a2, ToACN a3, ToACN a4, ToACN a5) => ToACN (a1,a2,a3,a4,a5) where
    toACN (a1,a2,a3,a4,a5) = [AcnSeqStart] ++ toACN a1 ++ toACN a2 ++ toACN a3 ++ toACN a4 ++ toACN a5 ++ [AcnSeqEnd]
instance (FromACN a1, FromACN a2, FromACN a3, FromACN a4, FromACN a5) => FromACN (a1,a2,a3,a4,a5) where
    fromACN (AcnSeqStart:r) = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r3) <- fromACN r2
        (a4, r4) <- fromACN r3
        (a5, r') <- fromACN r4
        case r' of
            AcnSeqEnd:l -> return ((a1,a2,a3,a4,a5), l)
            _           -> Left "expecting AcnSeqEnd"
instance (ToACN a1, ToACN a2, ToACN a3, ToACN a4, ToACN a5, ToACN a6) => ToACN (a1,a2,a3,a4,a5,a6) where
    toACN (a1,a2,a3,a4,a5,a6) = [AcnSeqStart] ++ toACN a1 ++ toACN a2 ++ toACN a3 ++ toACN a4 ++ toACN a5 ++ toACN a6 ++ [AcnSeqEnd]
instance (FromACN a1, FromACN a2, FromACN a3, FromACN a4, FromACN a5, FromACN a6) => FromACN (a1,a2,a3,a4,a5,a6) where
    fromACN (AcnSeqStart:r) = do
        (a1, r1) <- fromACN r
        (a2, r2) <- fromACN r1
        (a3, r3) <- fromACN r2
        (a4, r4) <- fromACN r3
        (a5, r5) <- fromACN r4
        (a6, r') <- fromACN r5
        case r' of
            AcnSeqEnd:l -> return ((a1,a2,a3,a4,a5,a6), l)
            _           -> Left "expecting AcnSeqEnd"
