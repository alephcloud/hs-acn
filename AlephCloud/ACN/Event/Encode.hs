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
