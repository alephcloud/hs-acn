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
