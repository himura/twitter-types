{-# LANGUAGE TemplateHaskell #-}

module PropFromToJSONTest where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Web.Twitter.Types

fromToJSON :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
fromToJSON obj = case fromJSON . toJSON $ obj of
    Aeson.Error _ -> False
    Aeson.Success a -> a == obj

prop_fromToStatus :: Status -> Bool
prop_fromToStatus = fromToJSON

prop_fromToSearchStatus :: SearchStatus -> Bool
prop_fromToSearchStatus = fromToJSON

prop_fromToSearchMetadata :: SearchMetadata -> Bool
prop_fromToSearchMetadata = fromToJSON

prop_fromToRetweetedStatus :: RetweetedStatus -> Bool
prop_fromToRetweetedStatus = fromToJSON

prop_fromToDirectMessage :: DirectMessage -> Bool
prop_fromToDirectMessage = fromToJSON

prop_fromToEventTarget :: EventTarget -> Bool
prop_fromToEventTarget = fromToJSON

prop_fromToEvent :: Event -> Bool
prop_fromToEvent = fromToJSON

prop_fromToDelete :: Delete -> Bool
prop_fromToDelete = fromToJSON

prop_fromToUser :: User -> Bool
prop_fromToUser = fromToJSON

prop_fromToList :: List -> Bool
prop_fromToList = fromToJSON

prop_fromToHashTagEntity :: HashTagEntity -> Bool
prop_fromToHashTagEntity = fromToJSON

prop_fromToUserEntity :: UserEntity -> Bool
prop_fromToUserEntity = fromToJSON

prop_fromToURLEntity :: URLEntity -> Bool
prop_fromToURLEntity = fromToJSON

prop_fromToMediaEntity :: MediaEntity -> Bool
prop_fromToMediaEntity = fromToJSON

prop_fromToMediaSize :: MediaSize -> Bool
prop_fromToMediaSize = fromToJSON

prop_fromToCoordinates :: Coordinates -> Bool
prop_fromToCoordinates = fromToJSON

prop_fromToPlace :: Place -> Bool
prop_fromToPlace = fromToJSON

prop_fromToBoundingBox :: BoundingBox -> Bool
prop_fromToBoundingBox = fromToJSON

prop_fromToEntities :: Entities -> Bool
prop_fromToEntities = fromToJSON

prop_fromToContributor :: Contributor -> Bool
prop_fromToContributor = fromToJSON

prop_fromToImageSizeType :: ImageSizeType -> Bool
prop_fromToImageSizeType = fromToJSON

prop_fromToUploadedMedia :: UploadedMedia -> Bool
prop_fromToUploadedMedia = fromToJSON

tests :: TestTree
tests = $(testGroupGenerator)
