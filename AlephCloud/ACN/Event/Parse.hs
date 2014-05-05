module AlephCloud.ACN.Event.Parse
    ( TaggedVal(..)
    , runParse
    ) where

import Control.Monad (liftM2)
import Data.Word
import Data.Binary (get)
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L

import AlephCloud.ACN.Event.Types

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

