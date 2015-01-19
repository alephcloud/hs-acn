-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Data.ACN.Debug
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
-- |
-- Module      : Data.ACN.Debug
-- Copyright   : (c) 2013-2014 PivotCloud, Inc
-- License     : Apache-2
-- Maintainer  : licensing@pivotmail.com
--

module Data.ACN.Debug
    ( acnPretty
    ) where

import           Data.ACN.Types
import           Control.Monad.Writer
import qualified Data.ByteString as B
import           Numeric

hexdump :: B.ByteString -> String
hexdump bs = concatMap hex $ B.unpack bs
    where hex n
            | n > 0xa   = showHex n ""
            | otherwise = "0" ++ showHex n ""

acnPretty :: [Acn] -> String
acnPretty = concat . execWriter . prettyPrint 0
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
    p (AcnInt i)     = string ("int: " ++ showHex i "")
    p (AcnBytes bs)  = string ("bytes: " ++ hexdump bs)
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
