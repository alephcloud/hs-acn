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
