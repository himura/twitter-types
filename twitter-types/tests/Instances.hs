{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Data.String
import Control.Applicative
import Data.DeriveTH
import qualified Data.Text as T
import Data.Time
import Test.QuickCheck
import Web.Twitter.Types
import Data.Aeson
import Data.HashMap.Strict as HashMap
import System.Locale

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

instance Arbitrary a => Arbitrary (Entity a) where
    arbitrary = do
        a <- arbitrary
        ind <- arbitrary
        return $ Entity a ind

derive makeArbitrary ''Contributor
derive makeArbitrary ''ImageSizeType
derive makeArbitrary ''UploadedMedia
