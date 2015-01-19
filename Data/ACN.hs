module Data.ACN
    ( module Data.ACN.Types
    , module Data.ACN.Event
    , module Data.ACN.Marshall
    , acnObjectFromBytes
    , acnObjectToBytes
    , acnStreamToBytes
    ) where

import Data.ACN.Types
import Data.ACN.Event
import Data.ACN.Marshall
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

acnObjectFromBytes :: FromACN a => B.ByteString -> Either String (a, [Acn])
acnObjectFromBytes =
    either (Left . snd) (maybe (Left "Invalid tagged val") fromACN . sequence . map fromTaggedVal) . runParse . L.fromStrict

acnObjectToBytes :: ToACN a => a -> B.ByteString
acnObjectToBytes = acnStreamToBytes . toACN

acnStreamToBytes :: [Acn] -> B.ByteString
acnStreamToBytes as = toByteString $ map toTaggedVal as
