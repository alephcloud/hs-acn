module AlephCloud.ACN.Debug
    ( acnPretty
    , acnPrettyWith
    ) where

import           AlephCloud.ACN.Types
import           Control.Monad.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Numeric

hexdump :: ByteString -> String
hexdump bs = concatMap hex $ B.unpack bs
    where hex n
            | n > 0xa   = showHex n ""
            | otherwise = "0" ++ showHex n ""

-- | Pretty printer with parameterized method printer
acnPrettyWith :: (ByteString -> String) -- ^ use a specific function to dump AcnBytes
              -> (Integer -> String)    -- ^ use a specific function to dump AcnInt
              -> [Acn]
              -> String
acnPrettyWith dumpByteString dumpInt = concat . execWriter . prettyPrint 0
  where
    indent n = string (replicate n ' ')

    prettyPrint :: Int -> [Acn] -> Writer [String] ()
    prettyPrint _ []                   = return ()
    prettyPrint n (x1@AcnSeqStart : x2@AcnSeqEnd : xs) = indent n >> p x1 >> p x2 >> newline >> prettyPrint n xs
    prettyPrint n (x1@AcnSetStart : x2@AcnSetEnd : xs) = indent n >> p x1 >> p x2 >> newline >> prettyPrint n xs
    prettyPrint n (x@AcnSeqStart : xs) = indent n >> p x >> newline >> prettyPrint (n+1) xs
    prettyPrint n (x@AcnSeqEnd : xs)   = indent (n-1) >> p x >> newline >> prettyPrint (n-1) xs
    prettyPrint n (x@AcnSetStart : xs) = indent n >> p x >> newline >> prettyPrint (n+1) xs
    prettyPrint n (x@AcnSetEnd : xs)   = indent (n-1) >> p x >> newline >> prettyPrint (n-1) xs
    prettyPrint n (x : xs)             = indent n >> p x >> newline >> prettyPrint n xs

    p (AcnBool b)    = string (show b)
    p (AcnInt i)     = string ("int: " ++ dumpInt i)
    p (AcnBytes bs)  = string ("bytes: " ++ dumpByteString bs)
    p (AcnID c)      = string ("id" ++ show c)
    p (AcnSeqStart)  = string "{"
    p (AcnSeqEnd)    = string "}"
    p (AcnSetStart)  = string "["
    p (AcnSetEnd)    = string "]"
    p (AcnString cs) = string cs

    newline :: Writer [String] ()
    newline = tell ["\n"]

    string :: String -> Writer [String] ()
    string s = tell [s]

-- | default pretty printer
--
-- dump methods show content in Hexadecimal (ByteString and Integer)
acnPretty :: [Acn] -> String
acnPretty = acnPrettyWith hexdump (\i -> showHex i "")
