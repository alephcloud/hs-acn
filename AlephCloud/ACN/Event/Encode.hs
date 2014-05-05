module AlephCloud.ACN.Event.Encode
    ( toLazyByteString
    ) where

import AlephCloud.ACN.Event.Types
import Data.Binary.Put (runPut)
import Data.Binary (put)
import qualified Data.ByteString.Lazy as L

toLazyByteString :: [TaggedVal] -> L.ByteString
toLazyByteString vals = runPut (mapM_ put vals)
