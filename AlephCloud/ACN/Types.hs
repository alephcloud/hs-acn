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
    | AcnSeqStart
    | AcnSeqEnd
    | AcnSetStart
    | AcnSetEnd
    deriving (Show,Eq)
