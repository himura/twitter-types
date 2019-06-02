{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Control.Applicative
import Data.Aeson
import Data.DeriveTH
import Data.HashMap.Strict as HashMap
import Data.String
import qualified Data.Text as T
import Data.Time (UTCTime (..), readTime, fromGregorian, defaultTimeLocale)
import Test.QuickCheck
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

instance Arbitrary LastStatus where
    arbitrary = do
        LastStatus <$> arbitrary
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
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

derive makeArbitrary ''SearchStatus
derive makeArbitrary ''SearchMetadata
derive makeArbitrary ''RetweetedStatus
derive makeArbitrary ''DirectMessage
derive makeArbitrary ''EventTarget
derive makeArbitrary ''Event
derive makeArbitrary ''Delete
derive makeArbitrary ''User
derive makeArbitrary ''List
derive makeArbitrary ''HashTagEntity
derive makeArbitrary ''UserEntity
derive makeArbitrary ''URLEntity

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

derive makeArbitrary ''MediaSize
derive makeArbitrary ''Coordinates

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

derive makeArbitrary ''BoundingBox
derive makeArbitrary ''Entities
derive makeArbitrary ''ExtendedEntities
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

instance Arbitrary a => Arbitrary (Entity a) where
    arbitrary = do
        a <- arbitrary
        ind <- arbitrary
        return $ Entity a ind

derive makeArbitrary ''Contributor
derive makeArbitrary ''ImageSizeType
derive makeArbitrary ''UploadedMedia
