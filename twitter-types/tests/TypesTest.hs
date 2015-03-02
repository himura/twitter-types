{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Web.Twitter.Types

import Test.Framework.TH.Prime
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as M
import Data.Maybe

import Instances()
import Fixtures
loadFixturesTH 'parseJSONValue

main :: IO ()
main = $(defaultMainGenerator)

withJSON :: FromJSON a
         => Value
         -> (a -> Assertion)
         -> Assertion
withJSON js f = either assertFailure id $ do
    o <- parseEither parseJSON js
    return $ f o

case_parseStatus :: Assertion
case_parseStatus = withJSON fixture_status01 $ \obj -> do
    statusCreatedAt obj @?= "Sat Sep 10 22:23:38 +0000 2011"
    statusId obj @?= 112652479837110273
    statusText obj @?= "@twitter meets @seepicturely at #tcdisrupt cc.@boscomonkey @episod http://t.co/6J2EgYM"
    statusSource obj @?= "<a href=\"http://instagr.am\" rel=\"nofollow\">Instagram</a>"
    statusTruncated obj @?= False
    statusEntities obj @?= Nothing
    statusExtendedEntities obj @?= Nothing
    statusInReplyToStatusId obj @?= Nothing
    statusInReplyToUserId obj @?= Just 783214
    statusFavorited obj @?= Just False
    statusRetweetCount obj @?= 0
    (userScreenName . statusUser) obj @?= "imeoin"
    statusRetweetedStatus obj @?= Nothing
    statusPlace obj @?= Nothing
    statusFavoriteCount obj @?= 0
    statusLang obj @?= Nothing
    statusPossiblySensitive obj @?= Just False
    statusCoordinates obj @?= Nothing

case_parseStatusWithPhoto :: Assertion
case_parseStatusWithPhoto = withJSON fixture_status_thimura_with_photo $ \obj -> do
    statusId obj @?= 491143410770657280
    statusText obj @?= "近所の海です http://t.co/FjSOU8dDoD"
    statusTruncated obj @?= False

    let ent = fromJust $ statusEntities obj
    enHashTags ent @?= []
    enUserMentions ent @?= []
    enURLs ent @?= []
    length (enMedia ent) @?= 1
    map (meMediaURLHttps . entityBody) (enMedia ent) @?= ["https://pbs.twimg.com/media/BtDkUVaCQAIpWBU.jpg"]

    let exent = fromJust $ statusExtendedEntities obj
    enHashTags exent @?= []
    enUserMentions exent @?= []
    enURLs exent @?= []
    length (enMedia ent) @?= 1

    statusInReplyToStatusId obj @?= Nothing
    statusInReplyToUserId obj @?= Nothing
    statusFavorited obj @?= Just False
    statusRetweetCount obj @?= 4
    (userScreenName . statusUser) obj @?= "thimura"
    statusRetweetedStatus obj @?= Nothing
    statusPlace obj @?= Nothing
    statusFavoriteCount obj @?= 9
    statusLang obj @?= Just "ja"
    statusPossiblySensitive obj @?= Just False
    statusCoordinates obj @?= Nothing

case_parseStatusIncludeEntities :: Assertion
case_parseStatusIncludeEntities = withJSON fixture_status_with_entity $ \obj -> do
    statusId obj @?= 112652479837110273
    statusRetweetCount obj @?= 0
    (userScreenName . statusUser) obj @?= "imeoin"
    let ent = fromMaybe (Entities [] [] [] []) $ statusEntities obj
    (map entityIndices . enHashTags) ent @?= [[32,42]]
    (hashTagText . entityBody . head . enHashTags) ent @?= "tcdisrupt"

case_parseSearchStatusMetadata :: Assertion
case_parseSearchStatusMetadata = withJSON fixture_search_haskell $ \obj -> do
    let status = (searchResultStatuses obj) :: [Status]
    length status @?= 1

    let metadata = searchResultSearchMetadata obj
    searchMetadataMaxId metadata @?= 495597397733433345
    searchMetadataSinceId metadata @?= 0
    searchMetadataRefreshURL metadata @?= "?since_id=495597397733433345&q=haskell&include_entities=1"
    searchMetadataNextResults metadata @?= Just "?max_id=495594369802440705&q=haskell&include_entities=1"
    searchMetadataCount metadata @?= 1
    searchMetadataCompletedIn metadata @?= Just 0.043
    searchMetadataSinceIdStr metadata @?= "0"
    searchMetadataQuery metadata @?= "haskell"
    searchMetadataMaxIdStr metadata @?= "495597397733433345"

case_parseSearchStatusBodyStatus :: Assertion
case_parseSearchStatusBodyStatus = withJSON fixture_search_haskell $ \obj -> do
    let status = (searchResultStatuses obj) :: [Status]
    length status @?= 1
    statusText (head status) @?= "haskell"

case_parseSearchStatusBodySearchStatus :: Assertion
case_parseSearchStatusBodySearchStatus = withJSON fixture_search_haskell $ \obj -> do
    let status = (searchResultStatuses obj) :: [SearchStatus]
    length status @?= 1
    searchStatusText (head status) @?= "haskell"

case_parseDirectMessage :: Assertion
case_parseDirectMessage = withJSON fixture_direct_message_thimura $ \obj -> do
    dmCreatedAt obj @?= "Sat Aug 02 16:10:04 +0000 2014"
    dmSenderScreenName obj @?= "thimura_shinku"
    (userScreenName . dmSender) obj @?= "thimura_shinku"
    dmText obj @?= "おまえの明日が、今日よりもずっと、楽しい事で溢れているようにと、祈っているよ"
    dmRecipientScreeName obj @?= "thimura"
    dmId obj @?= 495602442466123776
    (userScreenName . dmRecipient) obj @?= "thimura"
    dmRecipientId obj @?= 69179963
    dmSenderId obj @?= 2566877347
    dmCoordinates obj @?= Nothing

case_parseEventFavorite :: Assertion
case_parseEventFavorite = withJSON fixture_event_favorite_thimura $ \obj -> do
    evCreatedAt obj @?= "Sat Aug 02 16:32:01 +0000 2014"
    evEvent obj @?= "favorite"
    let Just (ETStatus targetObj) = evTargetObject obj
    statusId targetObj @?= 495597326736449536
    statusText targetObj @?= "haskell"

    let ETUser targetUser = evTarget obj
    userScreenName targetUser @?= "thimura"

    let ETUser sourceUser = evSource obj
    userScreenName sourceUser @?= "thimura_shinku"

case_parseEventUnfavorite :: Assertion
case_parseEventUnfavorite = withJSON fixture_event_unfavorite_thimura $ \obj -> do
    evCreatedAt obj @?= "Sat Aug 02 16:32:10 +0000 2014"
    evEvent obj @?= "unfavorite"
    let Just (ETStatus targetObj) = evTargetObject obj
    statusId targetObj @?= 495597326736449536
    statusText targetObj @?= "haskell"

    let ETUser targetUser = evTarget obj
    userScreenName targetUser @?= "thimura"

    let ETUser sourceUser = evSource obj
    userScreenName sourceUser @?= "thimura_shinku"

case_parseDelete :: Assertion
case_parseDelete = withJSON fixture_delete $ \obj -> do
    delId obj @?= 495607981833064448
    delUserId obj @?= 2566877347

case_parseErrorMsg :: Assertion
case_parseErrorMsg =
    case parseStatus fixture_error_not_authorized of
        Left str -> "Not authorized" @=? str
        Right _ -> assertFailure "errorMsgJson should be parsed as an error."
  where
    parseStatus :: Value -> Either String Status
    parseStatus = parseEither parseJSON

case_parseMediaEntity :: Assertion
case_parseMediaEntity = withJSON fixture_media_entity $ \obj -> do
    let entities = statusEntities obj
    assert $ isJust entities
    let Just ent = entities
        media = enMedia ent
    length media @?= 1
    let me = entityBody $ head media
    meType me @?= "photo"
    meId me @?= 114080493040967680
    let sizes = meSizes me
    assert $ M.member "thumb" sizes
    assert $ M.member "large" sizes

    let Just mediaSize = M.lookup "large" sizes

    msWidth mediaSize @?= 226
    msHeight mediaSize @?= 238
    msResize mediaSize @?= "fit"

    ueURL (meURL me) @?= "http://t.co/rJC5Pxsu"
    meMediaURLHttps me @?= "https://pbs.twimg.com/media/AZVLmp-CIAAbkyy.jpg"

case_parseEmptyEntity :: Assertion
case_parseEmptyEntity = withJSON (parseJSONValue "{}") $ \entity -> do
    length (enHashTags entity) @?= 0
    length (enUserMentions entity) @?= 0
    length (enURLs entity) @?= 0
    length (enMedia entity) @?= 0

case_parseEntityHashTag :: Assertion
case_parseEntityHashTag = withJSON fixture_entity01 $ \entity -> do
    length (enHashTags entity) @?= 1
    length (enUserMentions entity) @?= 1
    length (enURLs entity) @?= 1
    length (enMedia entity) @?= 0

    let urlEntity = entityBody . head . enURLs $ entity
    ueURL urlEntity @?= "http://t.co/IOwBrTZR"
    ueExpanded urlEntity @?= "http://www.youtube.com/watch?v=oHg5SJYRHA0"
    ueDisplay urlEntity @?= "youtube.com/watch?v=oHg5SJ\x2026"

    let mentionsUser = entityBody . head . enUserMentions $ entity
    userEntityUserName mentionsUser @?= "Twitter API"
    userEntityUserScreenName mentionsUser @?= "twitterapi"
    userEntityUserId mentionsUser @?= 6253282

    let HashTagEntity hashtag = entityBody . head . enHashTags $ entity
    hashtag @?= "lol"

case_parseExtendedEntities :: Assertion
case_parseExtendedEntities = withJSON fixture_media_extended_entity $ \obj -> do
    let entities = statusExtendedEntities obj
    assert $ isJust entities
    let Just ent = entities
        media = enMedia ent
    length media @?= 4
    let me = entityBody $ head media
    ueURL (meURL me) @?= "http://t.co/qOjPwmgLKO"
    meMediaURL me @?= "http://pbs.twimg.com/media/BqgdlpaCQAA5OSu.jpg"

case_parseUser :: Assertion
case_parseUser = withJSON fixture_user_thimura $ \obj -> do
    userId obj @?= 69179963
    userName obj @?= "ちむら"
    userScreenName obj @?= "thimura"
    userDescription obj @?= Just "真紅かわいい"
    userLocation obj @?= Just "State# Irotoridori.No.World"
    userProfileImageURL obj @?= Just "http://pbs.twimg.com/profile_images/414044387346116609/VNMfLpY7_normal.png"
    userURL obj @?= Just "http://t.co/TFUAsAffX0"
    userProtected obj @?= False
    userFollowersCount obj @?= 754
    userFriendsCount obj @?= 780
    userStatusesCount obj @?= 24709
    userLang obj @?= "en"
    userCreatedAt obj @?= "Thu Aug 27 02:48:06 +0000 2009"
    lastStatusCreatedAt (fromJust $ userStatus obj) @?= "Fri Aug 01 12:59:36 +0000 2014"
    userFavoritesCount obj @?= 17313

case_parseList :: Assertion
case_parseList = withJSON fixture_list_thimura_haskell $ \obj -> do
    listId obj @?= 20849097
    listName obj @?= "haskell"
    listFullName obj @?= "@thimura/haskell"
    listMemberCount obj @?= 50
    listSubscriberCount obj @?= 1
    listMode obj @?= "public"
    (userScreenName . listUser) obj @?= "thimura"

fromToJSON :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
fromToJSON obj = case fromJSON . toJSON $ obj of
    Aeson.Error _ -> False
    Aeson.Success a -> a == obj

-- prop_fromToStreamingAPI :: StreamingAPI -> Bool
-- prop_fromToStreamingAPI = fromToJSON

prop_fromToStatus :: Status -> Bool
prop_fromToStatus = fromToJSON

prop_fromToSearchStatus :: SearchStatus -> Bool
prop_fromToSearchStatus = fromToJSON

prop_fromToLastStatus :: LastStatus -> Bool
prop_fromToLastStatus = fromToJSON

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
