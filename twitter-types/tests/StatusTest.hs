{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module StatusTest where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Fixtures
import Instances()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Web.Twitter.Types

example_compatibility_classic_13995 :: Status -> Assertion
example_compatibility_classic_13995 obj = do
    statusCreatedAt obj @?= "Mon Mar 07 15:13:47 +0000 2016"
    statusId obj @?= 706860403981099008
    statusText obj @?= "Peek-a-boo! https://t.co/R3P6waHxRa"
    statusSource obj @?= "<a href=\"http://www.apple.com/\" rel=\"nofollow\">OS X</a>"
    statusTruncated obj @?= False
    statusEntities obj @?=
        Just
            (Entities
             { enHashTags = []
             , enUserMentions = []
             , enURLs = []
             , enMedia =
                   [ Entity
                     { entityBody =
                           MediaEntity
                           { meType = "photo"
                           , meId = 706860403746181121
                           , meSizes =
                                 M.fromList
                                     [ ( "small"
                                       , MediaSize
                                         { msWidth = 340
                                         , msHeight = 226
                                         , msResize = "fit"
                                         })
                                     , ( "large"
                                       , MediaSize
                                         { msWidth = 1024
                                         , msHeight = 680
                                         , msResize = "fit"
                                         })
                                     , ( "medium"
                                       , MediaSize
                                         { msWidth = 600
                                         , msHeight = 398
                                         , msResize = "fit"
                                         })
                                     , ( "thumb"
                                       , MediaSize
                                         { msWidth = 150
                                         , msHeight = 150
                                         , msResize = "crop"
                                         })
                                     ]
                           , meMediaURL = "http://pbs.twimg.com/media/Cc9FyscUkAEQaOw.jpg"
                           , meMediaURLHttps = "https://pbs.twimg.com/media/Cc9FyscUkAEQaOw.jpg"
                           , meURL =
                                 URLEntity
                                 { ueURL = "https://t.co/R3P6waHxRa"
                                 , ueExpanded = "http://twitter.com/jeremycloud/status/706860403981099008/photo/1"
                                 , ueDisplay = "pic.twitter.com/R3P6waHxRa"
                                 }
                           }
                     , entityIndices = [12, 35]
                     }
                   ]
             })
    statusExtendedEntities obj @?=
        Just
            (ExtendedEntities
             { exeMedia =
                   [ Entity
                     { entityBody =
                           ExtendedEntity
                           { exeID = 706860403746181121
                           , exeMediaUrl = "http://pbs.twimg.com/media/Cc9FyscUkAEQaOw.jpg"
                           , exeMediaUrlHttps = "https://pbs.twimg.com/media/Cc9FyscUkAEQaOw.jpg"
                           , exeSizes =
                                 M.fromList
                                     [ ( "small"
                                       , MediaSize
                                         { msWidth = 340
                                         , msHeight = 226
                                         , msResize = "fit"
                                         })
                                     , ( "large"
                                       , MediaSize
                                         { msWidth = 1024
                                         , msHeight = 680
                                         , msResize = "fit"
                                         })
                                     , ( "medium"
                                       , MediaSize
                                         { msWidth = 600
                                         , msHeight = 398
                                         , msResize = "fit"
                                         })
                                     , ( "thumb"
                                       , MediaSize
                                         { msWidth = 150
                                         , msHeight = 150
                                         , msResize = "crop"
                                         })
                                     ]
                           , exeType = "photo"
                           , exeDurationMillis = Nothing
                           , exeExtAltText = Nothing
                           , exeURL =
                                 URLEntity
                                 { ueURL = "https://t.co/R3P6waHxRa"
                                 , ueExpanded = "http://twitter.com/jeremycloud/status/706860403981099008/photo/1"
                                 , ueDisplay = "pic.twitter.com/R3P6waHxRa"
                                 }
                           }
                     , entityIndices = [12, 35]
                     }
                   ]
             })
    statusInReplyToStatusId obj @?= Nothing
    statusInReplyToUserId obj @?= Nothing
    statusFavorited obj @?= Just False
    statusQuotedStatus obj @?= Nothing
    statusQuotedStatusId obj @?= Nothing
    statusRetweetCount obj @?= 0
    (userScreenName . statusUser) obj @?= "jeremycloud"
    statusRetweetedStatus obj @?= Nothing
    statusPlace obj @?= Nothing
    statusFavoriteCount obj @?= 8
    statusLang obj @?= Just "en"
    statusPossiblySensitive obj @?= Just False
    statusCoordinates obj @?= Nothing

case_compatibility_classic_13995 :: Assertion
case_compatibility_classic_13995 = do
    withFixtureJSON "tweet-updates/compatibility_classic_13995.json" $ example_compatibility_classic_13995
    withFixtureJSON "tweet-updates/compatibility_classic_13995_extended.json" $ example_compatibility_classic_13995

-- case_compatibility_classic_hidden_13797 :: Assertion
-- case_compatibility_classic_hidden_13797 = withFixtureJSON "tweet-updates/compatibility_classic_hidden_13797" $ \obj -> do
-- case_compatibility_extended_13996 :: Assertion
-- case_compatibility_extended_13996 = withFixtureJSON "tweet-updates/compatibility_extended_13996" $ \obj -> do
-- case_compatibilityplus_classic_13994 :: Assertion
-- case_compatibilityplus_classic_13994 = withFixtureJSON "tweet-updates/compatibilityplus_classic_13994" $ \obj -> do
-- case_compatibilityplus_classic_hidden_13797 :: Assertion
-- case_compatibilityplus_classic_hidden_13797 = withFixtureJSON "tweet-updates/compatibilityplus_classic_hidden_13797" $ \obj -> do
-- case_compatibilityplus_extended_13997 :: Assertion
-- case_compatibilityplus_extended_13997 = withFixtureJSON "tweet-updates/compatibilityplus_extended_13997" $ \obj -> do
-- case_extended_classic_14002 :: Assertion
-- case_extended_classic_14002 = withFixtureJSON "tweet-updates/extended_classic_14002" $ \obj -> do
-- case_extended_classic_hidden_13761 :: Assertion
-- case_extended_classic_hidden_13761 = withFixtureJSON "tweet-updates/extended_classic_hidden_13761" $ \obj -> do

case_extended_extended_14001 :: Assertion
case_extended_extended_14001 = withFixtureJSON "tweet-updates/extended_extended_14001.json" $ \obj -> do
    statusText obj @?= "@twitter @twitterdev has more details about these changes at https://t.co/ZnXoRQy8mK.  Thanks for making @twitter more expressive! https://t.co/AWmiH870F7"
    statusTruncated obj @?= False
    statusDisplayTextRange obj @?= Just (DisplayTextRange 9 130)

tests :: TestTree
tests = $(testGroupGenerator)

