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
