module Main where

import Control.Applicative
import Control.Monad
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.ByteString (ByteString, pack)
import Data.ACN
import Data.Maybe

newtype AcnEvents = AcnEvents [TaggedVal]
    deriving (Show,Eq)

newtype AcnStream = AcnStream [Acn]
    deriving (Show,Eq)

instance Arbitrary ByteString where
    arbitrary = choose (0,49) >>= \sz ->
                pack <$> replicateM sz arbitrary

arbitraryID = (,) <$> arbitrary <*> choose (1,49512)

instance Arbitrary Acn where
    arbitrary = oneof
        [ AcnBytes  <$> arbitrary
        , AcnString <$> arbitrary
        , AcnBool   <$> arbitrary
        , AcnInt    <$> arbitrary
        , AcnID     <$> arbitraryID
        , pure AcnSeqStart
        , pure AcnSeqEnd
        , pure AcnSetStart
        , pure AcnSetEnd
        ]

instance Arbitrary AcnStream where
    arbitrary = AcnStream <$> (choose (1,58) >>= \sz -> replicateM sz arbitrary)

instance Arbitrary AcnEvents where
    arbitrary = AcnEvents <$> (pure [])

tests =
    [ testGroup "marshalling"
        [ testProperty "event" propMarshallingEvent
        , testProperty "test"  propMarshallingType
        , testProperty "types" propMarshallingTypes
        ]
    ]
  where propMarshallingEvent (AcnEvents l) = True
        propMarshallingType acn = Just acn `assertEQ` acn'
          where acn' = fromTaggedVal $ toTaggedVal acn
        propMarshallingTypes (AcnStream l) = l `assertEQ` l'
          where l' = catMaybes $ map fromTaggedVal $ map toTaggedVal l

assertEQ expected got
    | expected == got = True
    | otherwise       = error ("expected: " ++ show expected ++ " got: " ++ show got)

main = defaultMain $ testGroup "ACN" tests
