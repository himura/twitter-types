{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Control.Applicative
import Data.DeriveTH
import qualified Data.Text as T
import Test.QuickCheck
import Web.Twitter.Types
import Data.Aeson
import Data.HashMap.Strict as HashMap
import Data.Vector as V

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Value where
    arbitrary = elements [ Null, Bool True, Bool False
                         , Object HashMap.empty
                         , Array (V.fromList [Number 1.0, String "test"])
                         , String "test string"
                         ]

derive makeArbitrary ''StreamingAPI

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
