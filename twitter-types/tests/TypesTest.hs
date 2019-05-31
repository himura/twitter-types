{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TypesTest where

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

case_parseStatus :: Assertion
case_parseStatus = withFixtureJSON "status01.json" $ \obj -> do
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
    statusQuotedStatus obj @?= Nothing
    statusQuotedStatusId obj @?= Nothing
    statusRetweetCount obj @?= 0
    (userScreenName . statusUser) obj @?= "imeoin"
    statusRetweetedStatus obj @?= Nothing
    statusPlace obj @?= Nothing
    statusFavoriteCount obj @?= 0
    statusLang obj @?= Nothing
    statusPossiblySensitive obj @?= Just False
    statusCoordinates obj @?= Nothing

case_parseStatusQuoted :: Assertion
case_parseStatusQuoted = withFixtureJSON "status_quoted.json" $ \obj -> do
    statusId obj @?= 641660763770372100
    statusText obj @?= "Wow! Congrats! https://t.co/EPMMldEcci"
    statusQuotedStatusId obj @?= Just 641653574284537900

    let qs = fromJust $ statusQuotedStatus obj
    statusCreatedAt qs @?= "Wed Sep 09 16:45:08 +0000 2015"
    statusId qs @?= 641653574284537900
    statusText qs @?= "Very happy to say that I'm joining @mesosphere as a Distributed Systems Engineer!"
    statusSource qs @?= "<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>"

    let ent = fromJust $ statusEntities qs
    enURLs ent @?= []
    enMedia ent @?= []
    enHashTags ent @?= []
    map (userEntityUserId . entityBody) (enUserMentions ent) @?= [1872399366]
    map (userEntityUserScreenName . entityBody) (enUserMentions ent) @?= ["mesosphere"]

    statusExtendedEntities qs @?= Nothing
    statusInReplyToStatusId qs @?= Nothing
    statusInReplyToUserId qs @?= Nothing
    statusFavorited qs @?= Just False
    statusQuotedStatus qs @?= Nothing
    statusQuotedStatusId qs @?= Nothing
    statusRetweetCount qs @?= 7
    (userScreenName . statusUser) qs @?= "neil_conway"
    statusRetweeted qs @?= Just False
    statusRetweetedStatus qs @?= Nothing
    statusPlace qs @?= Nothing
    statusFavoriteCount qs @?= 63
    statusLang qs @?= Just "en"
    statusPossiblySensitive qs @?= Nothing
    statusCoordinates qs @?= Nothing


case_parseStatusWithPhoto :: Assertion
case_parseStatusWithPhoto = withFixtureJSON "status_thimura_with_photo.json" $ \obj -> do
    statusId obj @?= 491143410770657280
    statusText obj @?= "近所の海です http://t.co/FjSOU8dDoD"
    statusTruncated obj @?= False

    let ent = fromJust $ statusEntities obj
    enHashTags ent @?= []
    enUserMentions ent @?= []
    enURLs ent @?= []
    length (enMedia ent) @?= 1
    map (meMediaURLHttps . entityBody) (enMedia ent) @?= ["https://pbs.twimg.com/media/BtDkUVaCQAIpWBU.jpg"]

    let exents = fromJust $ statusExtendedEntities obj
    let media = exeMedia exents
    length media @?= 1
    let exent = entityBody $ head media
    exeID exent @?= 491143397378244610
    ueURL (exeURL exent) @?= "http://t.co/FjSOU8dDoD"

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
case_parseStatusIncludeEntities = withFixtureJSON "status_with_entity.json" $ \obj -> do
    statusId obj @?= 112652479837110273
    statusRetweetCount obj @?= 0
    (userScreenName . statusUser) obj @?= "imeoin"
    let ent = fromMaybe (Entities [] [] [] []) $ statusEntities obj
    (map entityIndices . enHashTags) ent @?= [[32,42]]
    (hashTagText . entityBody . head . enHashTags) ent @?= "tcdisrupt"

case_parseSearchStatusMetadata :: Assertion
case_parseSearchStatusMetadata = withFixtureJSON "search_haskell.json" $ \obj -> do
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
case_parseSearchStatusBodyStatus = withFixtureJSON "search_haskell.json" $ \obj -> do
    let status = (searchResultStatuses obj) :: [Status]
    length status @?= 1
    statusText (head status) @?= "haskell"

case_parseSearchStatusBodySearchStatus :: Assertion
case_parseSearchStatusBodySearchStatus = withFixtureJSON "search_haskell.json" $ \obj -> do
    let status = (searchResultStatuses obj) :: [SearchStatus]
    length status @?= 1
    searchStatusText (head status) @?= "haskell"

case_parseDirectMessage :: Assertion
case_parseDirectMessage = withFixtureJSON "direct_message_thimura.json" $ \obj -> do
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
case_parseEventFavorite = withFixtureJSON "event_favorite_thimura.json" $ \obj -> do
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
case_parseEventUnfavorite = withFixtureJSON "event_unfavorite_thimura.json" $ \obj -> do
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
case_parseDelete = withFixtureJSON "delete.json" $ \obj -> do
    delId obj @?= 495607981833064448
    delUserId obj @?= 2566877347

case_parseErrorMsg :: Assertion
case_parseErrorMsg = withFixtureJSON "error_not_authorized.json" $ \value ->
    case parseStatus value of
        Aeson.Error str -> "Not authorized" @=? str
        Aeson.Success _ -> assertFailure "errorMsgJson should be parsed as an error."
  where
    parseStatus :: Value -> Aeson.Result Status
    parseStatus = Aeson.parse parseJSON

case_parseMediaEntity :: Assertion
case_parseMediaEntity = withFixtureJSON "media_entity.json" $ \obj -> do
    let entities = statusEntities obj
    assertBool "entities should not empty" $ isJust entities
    let Just ent = entities
        media = enMedia ent
    length media @?= 1
    let me = entityBody $ head media
    meType me @?= "photo"
    meId me @?= 114080493040967680
    let sizes = meSizes me
    assertBool "sizes must contains \"thumb\"" $ M.member "thumb" sizes
    assertBool "sizes must contains \"large\"" $ M.member "large" sizes

    let Just mediaSize = M.lookup "large" sizes

    msWidth mediaSize @?= 226
    msHeight mediaSize @?= 238
    msResize mediaSize @?= "fit"

    ueURL (meURL me) @?= "http://t.co/rJC5Pxsu"
    meMediaURLHttps me @?= "https://pbs.twimg.com/media/AZVLmp-CIAAbkyy.jpg"

case_parseEmptyEntity :: Assertion
case_parseEmptyEntity = withJSON "{}" $ \entity -> do
    length (enHashTags entity) @?= 0
    length (enUserMentions entity) @?= 0
    length (enURLs entity) @?= 0
    length (enMedia entity) @?= 0

case_parseEntityHashTag :: Assertion
case_parseEntityHashTag = withFixtureJSON "entity01.json" $ \entity -> do
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
case_parseExtendedEntities = withFixtureJSON "media_extended_entity.json" $ \obj -> do
    let entities = statusExtendedEntities obj
    assertBool "entities should not empty" $ isJust entities
    let Just ent = entities
        media = exeMedia ent
    length media @?= 4
    let me = entityBody $ head media
    ueURL (exeURL me) @?= "https://t.co/Qi316FhOwe"
    exeMediaUrl me @?= "http://pbs.twimg.com/media/Coju86fUIAEUcRC.jpg"
    exeExtAltText me @?= Just "A small tabby kitten"
    exeType me @?= "photo"

case_parseUser :: Assertion
case_parseUser = withFixtureJSON "user_thimura.json" $ \obj -> do
    userId obj @?= 69179963
    userName obj @?= "ちむら"
    userScreenName obj @?= "thimura"
    userDescription obj @?= Just "真紅かわいい"
    userLocation obj @?= Just "State# Irotoridori.No.World"
    userProfileImageURLHttps obj @?= Just "https://pbs.twimg.com/profile_images/414044387346116609/VNMfLpY7_normal.png"
    userURL obj @?= Just "http://t.co/TFUAsAffX0"
    userProtected obj @?= False
    userFollowersCount obj @?= 754
    userFriendsCount obj @?= 780
    userStatusesCount obj @?= 24709
    userCreatedAt obj @?= "Thu Aug 27 02:48:06 +0000 2009"
    userFavoritesCount obj @?= 17313

case_parseList :: Assertion
case_parseList = withFixtureJSON "list_thimura_haskell.json" $ \obj -> do
    listId obj @?= 20849097
    listName obj @?= "haskell"
    listFullName obj @?= "@thimura/haskell"
    listMemberCount obj @?= 50
    listSubscriberCount obj @?= 1
    listMode obj @?= "public"
    (userScreenName . listUser) obj @?= "thimura"

tests :: TestTree
tests = $(testGroupGenerator)
