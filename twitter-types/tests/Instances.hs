{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Control.Applicative
import Data.Aeson
import Data.HashMap.Strict as HashMap
import Data.String
import qualified Data.Text as T
import Data.Time (UTCTime (..), readTime, fromGregorian, defaultTimeLocale)
import Generic.Random
import Test.Tasty.QuickCheck
import Web.Twitter.Types

instance IsString UTCTime where
    fromString = readTime defaultTimeLocale twitterTimeFormat

instance Arbitrary UTCTime where
    arbitrary =
        do randomDay <- choose (1, 29) :: Gen Int
           randomMonth <- choose (1, 12) :: Gen Int
           randomYear <- choose (2001, 2002) :: Gen Integer
           randomTime <- choose (0, 86401) :: Gen Int
           return $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Value where
    arbitrary = elements [ Object HashMap.empty
                         , Object (HashMap.fromList [("test", Number 2), ("value", String "non empty")])
                         ]

-- derive makeArbitrary ''StreamingAPI

instance Arbitrary Status where
    arbitrary = do
        qt <- frequency [(5, Just <$> arbitrary), (95, pure Nothing)] :: Gen (Maybe Status)
        rt <- frequency [(5, Just <$> arbitrary), (95, pure Nothing)] :: Gen (Maybe Status)
        Status <$> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> pure Nothing
               <*> pure (statusId <$> qt)
               <*> pure qt
               <*> arbitrary
               <*> arbitrary
               <*> pure rt
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary
               <*> arbitrary

instance Arbitrary SearchStatus where
    arbitrary = genericArbitraryU
instance Arbitrary SearchMetadata where
    arbitrary = genericArbitraryU
instance Arbitrary RetweetedStatus where
    arbitrary = genericArbitraryU

instance Arbitrary DirectMessage where
    arbitrary = genericArbitrarySingleG customGens
      where
        customGens :: Gen Integer :+ ()
        customGens =
            (getNonNegative <$> arbitrary) :+ ()

instance Arbitrary EventTarget where
    arbitrary = genericArbitraryU
instance Arbitrary Event where
    arbitrary = genericArbitraryU
instance Arbitrary Delete where
    arbitrary = genericArbitraryU
instance Arbitrary User where
    arbitrary = genericArbitraryU
instance Arbitrary List where
    arbitrary = genericArbitraryU
instance Arbitrary HashTagEntity where
    arbitrary = genericArbitraryU
instance Arbitrary UserEntity where
    arbitrary = genericArbitraryU
instance Arbitrary URLEntity where
    arbitrary = genericArbitraryU

instance Arbitrary MediaEntity where
    arbitrary = do
        ms <- arbitrary
        MediaEntity
            <$> arbitrary
            <*> arbitrary
            <*> pure (HashMap.fromList [("medium", ms)])
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary MediaSize where
    arbitrary = genericArbitraryU
instance Arbitrary Coordinates where
    arbitrary = genericArbitraryU

instance Arbitrary Place where
    arbitrary = do
        Place HashMap.empty
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary BoundingBox where
    arbitrary = genericArbitraryU
instance Arbitrary Entities where
    arbitrary = genericArbitraryU
instance Arbitrary ExtendedEntities where
    arbitrary = genericArbitraryU
instance Arbitrary Variant where
    arbitrary = genericArbitraryU
instance Arbitrary VideoInfo where
    arbitrary = genericArbitraryU
instance Arbitrary ExtendedEntity where
  arbitrary = do
    ms <- arbitrary
    ExtendedEntity
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure (HashMap.fromList [("medium", ms)])
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary a => Arbitrary (Entity a) where
    arbitrary = do
        a <- arbitrary
        ind <- arbitrary
        return $ Entity a ind

instance Arbitrary Contributor where
    arbitrary = genericArbitraryU
instance Arbitrary ImageSizeType where
    arbitrary = genericArbitraryU
instance Arbitrary UploadedMedia where
    arbitrary = genericArbitraryU
instance Arbitrary DisplayTextRange where
    arbitrary = genericArbitraryU
