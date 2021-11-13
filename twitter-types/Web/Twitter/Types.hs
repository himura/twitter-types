{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Twitter.Types
       ( UserId
       , Friends
       , URIString
       , UserName
       , StatusId
       , LanguageCode
       , StreamingAPI(..)
       , Status(..)
       , SearchResult(..)
       , SearchStatus(..)
       , SearchMetadata(..)
       , RetweetedStatus(..)
       , DirectMessage(..)
       , EventTarget(..)
       , Event(..)
       , Delete(..)
       , User(..)
       , List(..)
       , Entities(..)
       , EntityIndices
       , ExtendedEntities(..)
       , ExtendedEntity(..)
       , Entity(..)
       , HashTagEntity(..)
       , UserEntity(..)
       , URLEntity(..)
       , MediaEntity(..)
       , MediaSize(..)
       , Coordinates(..)
       , Place(..)
       , BoundingBox(..)
       , Contributor(..)
       , UploadedMedia (..)
       , ImageSizeType (..)
       , DisplayTextRange (..)
       , checkError
       , twitterTimeFormat
       )
       where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Data
import Data.HashMap.Strict (HashMap)
#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as KeyMap
#endif
import Data.Int
import Data.Ratio
import Data.Text (Text, unpack, pack)
import Data.Text.Read (decimal)
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics

newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }

type UserId       = Integer
type Friends      = [UserId]
type URIString    = Text
type UserName     = Text
type StatusId     = Integer
type LanguageCode = String

data StreamingAPI = SStatus Status
                  | SRetweetedStatus RetweetedStatus
                  | SEvent Event
                  | SDelete Delete
                  -- | SScrubGeo ScrubGeo
                  | SFriends Friends
                  | SDirectMessage DirectMessage
                  | SUnknown Value
                  deriving (Show, Eq, Data, Typeable, Generic)

checkError :: Object -> Parser ()
checkError o = do
    err <- o .:? "error"
    case err of
        Just msg -> fail msg
        Nothing -> return ()

twitterTimeFormat :: String
twitterTimeFormat = "%a %b %d %T %z %Y"

instance FromJSON TwitterTime where
    parseJSON = withText "TwitterTime" $ \t ->
        case parseTimeM True defaultTimeLocale twitterTimeFormat (unpack t) of
            Just  d -> pure $ TwitterTime d
            Nothing -> fail $ "Could not parse twitter time. Text was: " ++ unpack t

instance ToJSON TwitterTime where
    toJSON t = String $ pack $ formatTime defaultTimeLocale twitterTimeFormat $ fromTwitterTime t

instance FromJSON StreamingAPI where
    parseJSON v@(Object o) =
        SRetweetedStatus <$> js <|>
        SStatus <$> js <|>
        SEvent <$> js <|>
        SDelete <$> js <|>
        SFriends <$> (o .: "friends") <|>
        SDirectMessage <$> (o .: "direct_message") <|>
        return (SUnknown v)
      where
        js :: FromJSON a => Parser a
        js = parseJSON v
    parseJSON v = fail $ "couldn't parse StreamingAPI from: " ++ show v

instance ToJSON StreamingAPI where
    toJSON (SStatus          s) = toJSON s
    toJSON (SRetweetedStatus s) = toJSON s
    toJSON (SEvent           e) = toJSON e
    toJSON (SDelete          d) = toJSON d
    toJSON (SFriends         f) = toJSON f
    toJSON (SDirectMessage   m) = toJSON m
    toJSON (SUnknown         v) = v

-- | This type represents a Twitter tweet structure.
-- See <https://dev.twitter.com/docs/platform-objects/tweets>.
data Status = Status
    { statusContributors :: Maybe [Contributor]
    , statusCoordinates :: Maybe Coordinates
    , statusCreatedAt :: UTCTime
    , statusCurrentUserRetweet :: Maybe StatusId
    , statusEntities :: Maybe Entities
    , statusExtendedEntities :: Maybe ExtendedEntities
    , statusFavoriteCount :: Integer
    , statusFavorited :: Maybe Bool
    , statusFilterLevel :: Maybe Text
    , statusId :: StatusId
    , statusInReplyToScreenName :: Maybe Text
    , statusInReplyToStatusId :: Maybe StatusId
    , statusInReplyToUserId :: Maybe UserId
    , statusLang :: Maybe LanguageCode
    , statusPlace :: Maybe Place
    , statusPossiblySensitive :: Maybe Bool
    , statusScopes :: Maybe Object
    , statusQuotedStatusId :: Maybe StatusId
    , statusQuotedStatus :: Maybe Status
    , statusRetweetCount :: Integer
    , statusRetweeted :: Maybe Bool
    , statusRetweetedStatus :: Maybe Status
    , statusSource :: Text
    , statusText :: Text
    , statusTruncated :: Bool
    , statusUser :: User
    , statusWithheldCopyright :: Maybe Bool
    , statusWithheldInCountries :: Maybe [Text]
    , statusWithheldScope :: Maybe Text
    , statusDisplayTextRange :: Maybe DisplayTextRange
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Status where
    parseJSON (Object o) = checkError o >>
        Status <$> o .:? "contributors" .!= Nothing
               <*> o .:? "coordinates" .!= Nothing
               <*> (o .:  "created_at" >>= return . fromTwitterTime)
               <*> ((o .: "current_user_retweet" >>= (.: "id")) <|> return Nothing)
               <*> o .:? "entities"
               <*> o .:? "extended_entities"
               <*> o .:? "favorite_count" .!= 0
               <*> o .:? "favorited"
               <*> o .:? "filter_level"
               <*> o .:  "id"
               <*> o .:? "in_reply_to_screen_name" .!= Nothing
               <*> o .:? "in_reply_to_status_id" .!= Nothing
               <*> o .:? "in_reply_to_user_id" .!= Nothing
               <*> o .:? "lang"
               <*> o .:? "place" .!= Nothing
               <*> o .:? "possibly_sensitive"
               <*> o .:? "scopes"
               <*> o .:? "quoted_status_id"
               <*> o .:? "quoted_status"
               <*> o .:? "retweet_count" .!= 0
               <*> o .:? "retweeted"
               <*> o .:? "retweeted_status"
               <*> o .:  "source"
               <*> (o .: "full_text" <|> o .: "text")
               <*> o .:  "truncated"
               <*> o .:  "user"
               <*> o .:? "withheld_copyright"
               <*> o .:? "withheld_in_countries"
               <*> o .:? "withheld_scope"
               <*> o .:? "display_text_range"
    parseJSON v = fail $ "couldn't parse status from: " ++ show v

instance ToJSON Status where
    toJSON Status{..} = object [ "contributors"             .= statusContributors
                               , "coordinates"              .= statusCoordinates
                               , "created_at"               .= TwitterTime statusCreatedAt
                               , "current_user_retweet"     .= object [ "id"     .= statusCurrentUserRetweet
                                                                      , "id_str" .= show statusCurrentUserRetweet
                                                                      ]
                               , "entities"                 .= statusEntities
                               , "extended_entities"        .= statusExtendedEntities
                               , "favorite_count"           .= statusFavoriteCount
                               , "favorited"                .= statusFavorited
                               , "filter_level"             .= statusFilterLevel
                               , "id"                       .= statusId
                               , "in_reply_to_screen_name"  .= statusInReplyToScreenName
                               , "in_reply_to_status_id"    .= statusInReplyToStatusId
                               , "in_reply_to_user_id"      .= statusInReplyToUserId
                               , "lang"                     .= statusLang
                               , "place"                    .= statusPlace
                               , "possibly_sensitive"       .= statusPossiblySensitive
                               , "scopes"                   .= statusScopes
                               , "quoted_status_id"         .= statusQuotedStatusId
                               , "quoted_status"            .= statusQuotedStatus
                               , "retweet_count"            .= statusRetweetCount
                               , "retweeted"                .= statusRetweeted
                               , "retweeted_status"         .= statusRetweetedStatus
                               , "source"                   .= statusSource
                               , "text"                     .= statusText
                               , "truncated"                .= statusTruncated
                               , "user"                     .= statusUser
                               , "withheld_copyright"       .= statusWithheldCopyright
                               , "withheld_in_countries"    .= statusWithheldInCountries
                               , "withheld_scope"           .= statusWithheldScope
                               , "display_text_range"       .= statusDisplayTextRange
                               ]

data SearchResult body =
    SearchResult
    { searchResultStatuses :: body
    , searchResultSearchMetadata :: SearchMetadata
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON body =>
         FromJSON (SearchResult body) where
    parseJSON (Object o) = checkError o >>
        SearchResult <$> o .:  "statuses"
                     <*> o .:  "search_metadata"
    parseJSON v = fail $ "couldn't parse search result from: " ++ show v

instance ToJSON body =>
         ToJSON (SearchResult body) where
    toJSON SearchResult{..} = object [ "statuses"        .= searchResultStatuses
                                     , "search_metadata" .= searchResultSearchMetadata
                                     ]

data SearchStatus =
    SearchStatus
    { searchStatusCreatedAt     :: UTCTime
    , searchStatusId            :: StatusId
    , searchStatusText          :: Text
    , searchStatusSource        :: Text
    , searchStatusUser          :: User
    , searchStatusCoordinates   :: Maybe Coordinates
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON SearchStatus where
    parseJSON (Object o) = checkError o >>
        SearchStatus <$> (o .:  "created_at" >>= return . fromTwitterTime)
                     <*> o .:  "id"
                     <*> o .:  "text"
                     <*> o .:  "source"
                     <*> o .:  "user"
                     <*> o .:? "coordinates"
    parseJSON v = fail $ "couldn't parse status search result from: " ++ show v

instance ToJSON SearchStatus where
    toJSON SearchStatus{..} = object [ "created_at"     .= TwitterTime searchStatusCreatedAt
                                     , "id"             .= searchStatusId
                                     , "text"           .= searchStatusText
                                     , "source"         .= searchStatusSource
                                     , "user"           .= searchStatusUser
                                     , "coordinates"    .= searchStatusCoordinates
                                     ]

data SearchMetadata =
    SearchMetadata
    { searchMetadataMaxId         :: StatusId
    , searchMetadataSinceId       :: StatusId
    , searchMetadataRefreshURL    :: URIString
    , searchMetadataNextResults   :: Maybe URIString
    , searchMetadataCount         :: Int
    , searchMetadataCompletedIn   :: Maybe Float
    , searchMetadataSinceIdStr    :: String
    , searchMetadataQuery         :: String
    , searchMetadataMaxIdStr      :: String
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON SearchMetadata where
    parseJSON (Object o) = checkError o >>
        SearchMetadata <$> o .:  "max_id"
                       <*> o .:  "since_id"
                       <*> o .:  "refresh_url"
                       <*> o .:? "next_results"
                       <*> o .:  "count"
                       <*> o .:? "completed_in"
                       <*> o .:  "since_id_str"
                       <*> o .:  "query"
                       <*> o .:  "max_id_str"
    parseJSON v = fail $ "couldn't parse search metadata from: " ++ show v

instance ToJSON SearchMetadata where
    toJSON SearchMetadata{..} = object [ "max_id"       .= searchMetadataMaxId
                                       , "since_id"     .= searchMetadataSinceId
                                       , "refresh_url"  .= searchMetadataRefreshURL
                                       , "next_results" .= searchMetadataNextResults
                                       , "count"        .= searchMetadataCount
                                       , "completed_in" .= searchMetadataCompletedIn
                                       , "since_id_str" .= searchMetadataSinceIdStr
                                       , "query"        .= searchMetadataQuery
                                       , "max_id_str"   .= searchMetadataMaxIdStr
                                       ]

data RetweetedStatus =
    RetweetedStatus
    { rsCreatedAt       :: UTCTime
    , rsId              :: StatusId
    , rsText            :: Text
    , rsSource          :: Text
    , rsTruncated       :: Bool
    , rsEntities        :: Maybe Entities
    , rsUser            :: User
    , rsRetweetedStatus :: Status
    , rsCoordinates     :: Maybe Coordinates
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON RetweetedStatus where
    parseJSON (Object o) = checkError o >>
        RetweetedStatus <$> (o .:  "created_at" >>= return . fromTwitterTime)
                        <*> o .:  "id"
                        <*> o .:  "text"
                        <*> o .:  "source"
                        <*> o .:  "truncated"
                        <*> o .:? "entities"
                        <*> o .:  "user"
                        <*> o .:  "retweeted_status"
                        <*> o .:? "coordinates"
    parseJSON v = fail $ "couldn't parse retweeted status from: " ++ show v

instance ToJSON RetweetedStatus where
    toJSON RetweetedStatus{..} = object [ "created_at"          .= TwitterTime rsCreatedAt
                                        , "id"                  .= rsId
                                        , "text"                .= rsText
                                        , "source"              .= rsSource
                                        , "truncated"           .= rsTruncated
                                        , "entities"            .= rsEntities
                                        , "user"                .= rsUser
                                        , "retweeted_status"    .= rsRetweetedStatus
                                        , "coordinates"         .= rsCoordinates
                                        ]

type EventId = Integer

data DirectMessage = DirectMessage
    { dmId :: EventId
    , dmCreatedTimestamp :: UTCTime
    , dmTargetRecipientId :: UserId
    , dmSenderId :: UserId
    , dmText :: Text
    , dmEntities :: Entities
    } deriving (Show, Eq, Data, Typeable, Generic)

parseIntegral :: Integral a => Text -> Parser a
parseIntegral v = either (\_ -> fail $ "couldn't parse stringized int: " ++ show v) (return . fst) $ decimal v

epochMsToUTCTime :: Int64 -> UTCTime
epochMsToUTCTime = posixSecondsToUTCTime . fromRational . (% 1000) . fromIntegral

parseUnixTimeString :: Text -> Parser UTCTime
parseUnixTimeString = fmap epochMsToUTCTime <$> parseIntegral

unixTimeToEpochInt :: UTCTime -> Int
unixTimeToEpochInt = floor . (* 1000) . utcTimeToPOSIXSeconds

instance FromJSON DirectMessage where
    parseJSON (Object o) = do
        _ <- checkError o
        messageCreate <- o .: "message_create"
        messageData <- messageCreate .: "message_data"

        DirectMessage
            <$> (o .: "id" >>= parseIntegral)
            <*> (o .: "created_timestamp" >>= parseUnixTimeString)
            <*> (messageCreate .: "target" >>= (.: "recipient_id") >>= parseIntegral)
            <*> (messageCreate .: "sender_id" >>= parseIntegral)
            <*> messageData .: "text"
            <*> messageData .: "entities"
    parseJSON v = fail $ "couldn't parse direct message create event from: " ++ show v

instance ToJSON DirectMessage where
    toJSON DirectMessage {..} =
        object
            [ "id" .= show dmId
            , "created_timestamp" .= show (unixTimeToEpochInt dmCreatedTimestamp)
            , "message_create" .= object ["message_data" .= object ["text" .= dmText, "entities" .= dmEntities]
                                         , "target" .= object ["recipient_id" .= show dmTargetRecipientId]
                                         , "sender_id" .= show dmSenderId
                                         ]
            ]

data EventType = Favorite | Unfavorite
               | ListCreated | ListUpdated | ListMemberAdded
               | UserUpdate | Block | Unblock | Follow
               deriving (Show, Eq, Data, Typeable, Generic)

data EventTarget = ETUser User | ETStatus Status | ETList List | ETUnknown Value
                 deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON EventTarget where
    parseJSON v@(Object o) = checkError o >>
        ETUser <$> parseJSON v <|>
        ETStatus <$> parseJSON v <|>
        ETList <$> parseJSON v <|>
        return (ETUnknown v)
    parseJSON v = fail $ "couldn't parse event target from: " ++ show v

instance ToJSON EventTarget where
    toJSON (ETUser    u) = toJSON u
    toJSON (ETStatus  s) = toJSON s
    toJSON (ETList    l) = toJSON l
    toJSON (ETUnknown v) = v

data Event =
    Event
    { evCreatedAt       :: UTCTime
    , evTargetObject    :: Maybe EventTarget
    , evEvent           :: Text
    , evTarget          :: EventTarget
    , evSource          :: EventTarget
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Event where
    parseJSON (Object o) = checkError o >>
        Event <$> (o .:  "created_at" >>= return . fromTwitterTime)
              <*> o .:? "target_object"
              <*> o .:  "event"
              <*> o .:  "target"
              <*> o .:  "source"
    parseJSON v = fail $ "couldn't parse event from: " ++ show v

instance ToJSON Event where
    toJSON Event{..} = object [ "created_at"    .= TwitterTime evCreatedAt
                              , "target_object" .= evTargetObject
                              , "event"         .= evEvent
                              , "target"        .= evTarget
                              , "source"        .= evSource
                              ]

data Delete =
    Delete
    { delId  :: StatusId
    , delUserId :: UserId
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Delete where
    parseJSON (Object o) = checkError o >> do
        s <- o .: "delete" >>= (.: "status")
        Delete <$> s .: "id"
               <*> s .: "user_id"
    parseJSON v = fail $ "couldn't parse delete from: " ++ show v

instance ToJSON Delete where
    toJSON Delete{..} = object [ "delete" .= object [ "status" .= object [ "id"      .= delId
                                                                         , "user_id" .= delUserId
                                                                         ]
                                                    ]
                               ]

-- | This type represents the Twitter user.
-- See <https://dev.twitter.com/docs/platform-objects/users>.
data User = User
    { userCreatedAt :: UTCTime
    , userDefaultProfile :: Bool
    , userDefaultProfileImage :: Bool
    , userDescription :: Maybe Text
    , userFavoritesCount :: Int
    , userFollowersCount :: Int
    , userFriendsCount :: Int
    , userId :: UserId
    , userListedCount :: Int
    , userLocation :: Maybe Text
    , userName :: Text
    , userProfileBannerURL :: Maybe URIString
    , userProfileImageURLHttps :: Maybe URIString
    , userProtected :: Bool
    , userScreenName :: Text
    , userShowAllInlineMedia :: Maybe Bool
    , userStatusesCount :: Int
    , userURL :: Maybe URIString
    , userVerified :: Bool
    , userWithheldInCountries :: Maybe [Text]
    , userWithheldScope :: Maybe Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON User where
    parseJSON (Object o) = checkError o >>
        User <$> (fromTwitterTime <$> (o .: "created_at"))
             <*> o .:  "default_profile"
             <*> o .:  "default_profile_image"
             <*> o .:? "description"
             <*> o .:  "favourites_count"
             <*> o .:  "followers_count"
             <*> o .:  "friends_count"
             <*> o .:  "id"
             <*> o .:  "listed_count"
             <*> o .:? "location"
             <*> o .:  "name"
             <*> o .:? "profile_banner_url"
             <*> o .:? "profile_image_url_https"
             <*> o .:  "protected"
             <*> o .:  "screen_name"
             <*> o .:? "show_all_inline_media"
             <*> o .:  "statuses_count"
             <*> o .:? "url" .!= Nothing
             <*> o .:  "verified"
             <*> o .:? "withheld_in_countries"
             <*> o .:? "withheld_scope"
    parseJSON v = fail $ "couldn't parse user from: " ++ show v

instance ToJSON User where
    toJSON User{..} = object [ "created_at"                         .= TwitterTime userCreatedAt
                             , "default_profile"                    .= userDefaultProfile
                             , "default_profile_image"              .= userDefaultProfileImage
                             , "description"                        .= userDescription
                             , "favourites_count"                   .= userFavoritesCount
                             , "followers_count"                    .= userFollowersCount
                             , "friends_count"                      .= userFriendsCount
                             , "id"                                 .= userId
                             , "listed_count"                       .= userListedCount
                             , "location"                           .= userLocation
                             , "name"                               .= userName
                             , "profile_banner_url"                 .= userProfileBannerURL
                             , "profile_image_url_https"            .= userProfileImageURLHttps
                             , "protected"                          .= userProtected
                             , "screen_name"                        .= userScreenName
                             , "show_all_inline_media"              .= userShowAllInlineMedia
                             , "statuses_count"                     .= userStatusesCount
                             , "url"                                .= userURL
                             , "verified"                           .= userVerified
                             , "withheld_in_countries"              .= userWithheldInCountries
                             , "withheld_scope"                     .= userWithheldScope
                             ]

data List =
    List
    { listId :: Int
    , listName :: Text
    , listFullName :: Text
    , listMemberCount :: Int
    , listSubscriberCount :: Int
    , listMode :: Text
    , listUser :: User
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON List where
    parseJSON (Object o) = checkError o >>
        List <$> o .:  "id"
             <*> o .:  "name"
             <*> o .:  "full_name"
             <*> o .:  "member_count"
             <*> o .:  "subscriber_count"
             <*> o .:  "mode"
             <*> o .:  "user"
    parseJSON v = fail $ "couldn't parse List from: " ++ show v

instance ToJSON List where
    toJSON List{..} = object [ "id"                 .= listId
                             , "name"               .= listName
                             , "full_name"          .= listFullName
                             , "member_count"       .= listMemberCount
                             , "subscriber_count"   .= listSubscriberCount
                             , "mode"               .= listMode
                             , "user"               .= listUser
                             ]

-- | Hashtag entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-hashtags>.
data HashTagEntity =
    HashTagEntity
    { hashTagText :: Text -- ^ The Hashtag text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON HashTagEntity where
    parseJSON (Object o) =
        HashTagEntity <$> o .: "text"
    parseJSON v = fail $ "couldn't parse hashtag entity from: " ++ show v

instance ToJSON HashTagEntity where
    toJSON HashTagEntity{..} = object [ "text" .= hashTagText ]

-- | User mention entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-usermention>.
data UserEntity =
    UserEntity
    { userEntityUserId              :: UserId
    , userEntityUserName            :: UserName
    , userEntityUserScreenName      :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON UserEntity where
    parseJSON (Object o) =
        UserEntity <$> o .:  "id"
                   <*> o .:  "name"
                   <*> o .:  "screen_name"
    parseJSON v = fail $ "couldn't parse user entity from: " ++ show v

instance ToJSON UserEntity where
    toJSON UserEntity{..} = object [ "id"           .= userEntityUserId
                                   , "name"         .= userEntityUserName
                                   , "screen_name"  .= userEntityUserScreenName
                                   ]

-- | URL entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-url>.
data URLEntity =
    URLEntity
    { ueURL      :: URIString -- ^ The URL that was extracted
    , ueExpanded :: URIString -- ^ The fully resolved URL (only for t.co links)
    , ueDisplay  :: Text    -- ^ Not a URL but a string to display instead of the URL (only for t.co links)
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON URLEntity where
    parseJSON (Object o) =
        URLEntity <$> o .:  "url"
                  <*> o .:  "expanded_url"
                  <*> o .:  "display_url"
    parseJSON v = fail $ "couldn't parse url entity from: " ++ show v

instance ToJSON URLEntity where
    toJSON URLEntity{..} = object [ "url"           .= ueURL
                                  , "expanded_url"  .= ueExpanded
                                  , "display_url"   .= ueDisplay
                                  ]

data MediaEntity =
    MediaEntity
    { meType :: Text
    , meId :: StatusId
    , meSizes :: HashMap Text MediaSize
    , meMediaURL :: URIString
    , meMediaURLHttps :: URIString
    , meURL :: URLEntity
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON MediaEntity where
    parseJSON v@(Object o) =
        MediaEntity <$> o .: "type"
                    <*> o .: "id"
                    <*> o .: "sizes"
                    <*> o .: "media_url"
                    <*> o .: "media_url_https"
                    <*> parseJSON v
    parseJSON v = fail $ "couldn't parse media entity from: " ++ show v

instance ToJSON MediaEntity where
    toJSON MediaEntity{..} = object [ "type"            .= meType
                                    , "id"              .= meId
                                    , "sizes"           .= meSizes
                                    , "media_url"       .= meMediaURL
                                    , "media_url_https" .= meMediaURLHttps
                                    , "url"             .= ueURL meURL
                                    , "expanded_url"    .= ueExpanded meURL
                                    , "display_url"     .= ueDisplay meURL
                                    ]

-- | Size entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-size>.
data MediaSize =
    MediaSize
    { msWidth :: Int
    , msHeight :: Int
    , msResize :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON MediaSize where
    parseJSON (Object o) =
        MediaSize <$> o .: "w"
                  <*> o .: "h"
                  <*> o .: "resize"
    parseJSON v = fail $ "couldn't parse media size from: " ++ show v

instance ToJSON MediaSize where
    toJSON MediaSize{..} = object [ "w"      .= msWidth
                                  , "h"      .= msHeight
                                  , "resize" .= msResize
                                  ]

data Coordinates =
    Coordinates
    { coordinates :: [Double]
    , coordinatesType :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Coordinates where
    parseJSON (Object o) =
        Coordinates <$> o .: "coordinates"
                    <*> o .: "type"
    parseJSON v = fail $ "couldn't parse coordinates from: " ++ show v

instance ToJSON Coordinates where
    toJSON Coordinates{..} = object [ "coordinates" .= coordinates
                                    , "type"        .= coordinatesType
                                    ]

-- | This type represents a place, named locations with corresponding geo coordinates.
-- See <https://dev.twitter.com/docs/platform-objects/places>.
data Place =
    Place
    { placeAttributes   :: HashMap Text Text
    , placeBoundingBox  :: Maybe BoundingBox
    , placeCountry      :: Text
    , placeCountryCode  :: Text
    , placeFullName     :: Text
    , placeId           :: Text
    , placeName         :: Text
    , placeType         :: Text
    , placeURL          :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Place where
    parseJSON (Object o) =
        Place <$> o .: "attributes"
              <*> o .:? "bounding_box"
              <*> o .: "country"
              <*> o .: "country_code"
              <*> o .: "full_name"
              <*> o .: "id"
              <*> o .: "name"
              <*> o .: "place_type"
              <*> o .: "url"
    parseJSON v = fail $ "couldn't parse place from: " ++ show v

instance ToJSON Place where
    toJSON Place{..} = object [ "attributes"    .= placeAttributes
                              , "bounding_box"  .= placeBoundingBox
                              , "country"       .= placeCountry
                              , "country_code"  .= placeCountryCode
                              , "full_name"     .= placeFullName
                              , "id"            .= placeId
                              , "name"          .= placeName
                              , "place_type"    .= placeType
                              , "url"           .= placeURL
                              ]

-- | A bounding box of coordinates which encloses the place.
-- See <https://dev.twitter.com/docs/platform-objects/places#obj-boundingbox>.
data BoundingBox =
    BoundingBox
    { boundingBoxCoordinates  :: [[[Double]]]
    , boundingBoxType         :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON BoundingBox where
    parseJSON (Object o) =
        BoundingBox <$> o .: "coordinates"
                    <*> o .: "type"
    parseJSON v = fail $ "couldn't parse bounding box from: " ++ show v

instance ToJSON BoundingBox where
    toJSON BoundingBox{..} = object [ "coordinates" .= boundingBoxCoordinates
                                    , "type"        .= boundingBoxType
                                    ]

-- | Entity handling.
-- See <https://dev.twitter.com/docs/platform-objects/entities>.
data Entities =
    Entities
    { enHashTags     :: [Entity HashTagEntity]
    , enUserMentions :: [Entity UserEntity]
    , enURLs         :: [Entity URLEntity]
    , enMedia        :: [Entity MediaEntity]
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Entities where
    parseJSON (Object o) =
        Entities <$> o .:? "hashtags" .!= []
                 <*> o .:? "user_mentions" .!= []
                 <*> o .:? "urls" .!= []
                 <*> o .:? "media" .!= []
    parseJSON v = fail $ "couldn't parse entities from: " ++ show v

instance ToJSON Entities where
    toJSON Entities{..} = object [ "hashtags"       .= enHashTags
                                 , "user_mentions"  .= enUserMentions
                                 , "urls"           .= enURLs
                                 , "media"          .= enMedia
                                 ]


-- | The character positions the Entity was extracted from
--
--   This is experimental implementation.
--   This may be replaced by more definite types.
type EntityIndices = [Int]

data Entity a =
    Entity
    { entityBody    :: a             -- ^ The detail information of the specific entity types (HashTag, URL, User)
    , entityIndices :: EntityIndices -- ^ The character positions the Entity was extracted from
    } deriving (Show, Eq, Data, Typeable, Generic, Generic1)

instance FromJSON a => FromJSON (Entity a) where
    parseJSON v@(Object o) =
        Entity <$> parseJSON v
               <*> o .: "indices"
    parseJSON v = fail $ "couldn't parse entity wrapper from: " ++ show v

instance ToJSON a => ToJSON (Entity a) where
    toJSON Entity {..} =
        case toJSON entityBody of
            (Object o) -> Object $ KeyMap.insert "indices" (toJSON entityIndices) o
            _ -> error "Entity body must produce an object."

data ExtendedEntities =
  ExtendedEntities {
    exeMedia :: [Entity ExtendedEntity]
  } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON ExtendedEntities where
  parseJSON (Object o) =
    ExtendedEntities <$> o .:? "media" .!= []

  parseJSON v = fail $ "couldn't parse extended entity from: " ++ show v

instance ToJSON ExtendedEntities where
    toJSON ExtendedEntities{..} = object [ "media" .= exeMedia ]


-- Extended entities are like entities, but contain special media features like
-- video or multiple photos
data ExtendedEntity =
  ExtendedEntity
  {
    exeID :: StatusId,
    exeMediaUrl :: URIString,
    exeMediaUrlHttps :: URIString,
    exeSizes :: HashMap Text MediaSize,
    exeType :: Text,
    -- exeVideoInfo :: ???
    exeDurationMillis :: Maybe Double,
    -- exeVariants :: ??
    exeExtAltText :: Maybe String,
    exeURL :: URLEntity
  } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON ExtendedEntity where
  parseJSON v@(Object o) =
    ExtendedEntity <$> o .:  "id"
                   <*> o .:  "media_url"
                   <*> o .:  "media_url_https"
                   <*> o .:  "sizes"
                   <*> o .:  "type"
                   <*> o .:? "duration_millis"
                   <*> o .:?  "ext_alt_text"
                   <*> parseJSON v

  parseJSON v = fail $ "couldn't parse extended entity from:" ++ show v

instance ToJSON ExtendedEntity where
  toJSON ExtendedEntity{..} = object [ "id"                .= exeID
                                     , "media_url"       .= exeMediaUrl
                                     , "media_url_https" .= exeMediaUrlHttps
                                     , "sizes"           .= exeSizes
                                     , "type"            .= exeType
                                     , "duration_millis" .= exeDurationMillis
                                     , "ext_alt_text"    .= exeExtAltText
                                     , "url"             .= ueURL exeURL
                                     , "expanded_url"    .= ueExpanded exeURL
                                     , "display_url"     .= ueDisplay exeURL
                                     ]


data Contributor = Contributor
    { contributorId :: UserId
    , contributorScreenName :: Maybe Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Contributor where
    parseJSON (Object o) =
        Contributor <$>  o .:  "id"
                    <*>  o .:?  "screen_name"
    parseJSON v@(Number _) =
        Contributor <$> parseJSON v <*> pure Nothing
    parseJSON v = fail $ "couldn't parse contributor from: " ++ show v

instance ToJSON Contributor where
    toJSON Contributor{..} = object [ "id"          .= contributorId
                                    , "screen_name" .= contributorScreenName
                                    ]

-- | Image size type. This type is included in the API response of \"\/1.1\/media\/upload.json\".
data ImageSizeType = ImageSizeType
    { imageSizeTypeWidth :: Int
    , imageSizeTypeHeight :: Int
    , imageSizeTypeType :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON ImageSizeType where
    parseJSON (Object o) =
        ImageSizeType <$> o .:  "w"
                      <*> o .:  "h"
                      <*> o .:  "image_type"
    parseJSON v = fail $ "unknown value: " ++ show v

instance ToJSON ImageSizeType where
    toJSON ImageSizeType{..} = object [ "w"          .= imageSizeTypeWidth
                                      , "h"          .= imageSizeTypeHeight
                                      , "image_type" .= imageSizeTypeType
                                      ]

-- | This type is represents the API response of \"\/1.1\/media\/upload.json\".
-- See <https://dev.twitter.com/docs/api/multiple-media-extended-entities>.
data UploadedMedia = UploadedMedia
    { uploadedMediaId :: Integer
    , uploadedMediaSize :: Integer
    , uploadedMediaImage :: ImageSizeType
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON UploadedMedia where
    parseJSON (Object o) =
        UploadedMedia <$> o .:  "media_id"
                      <*> o .:  "size"
                      <*> o .:  "image"
    parseJSON v = fail $ "unknown value: " ++ show v

instance ToJSON UploadedMedia where
    toJSON UploadedMedia{..} = object [ "media_id"  .= uploadedMediaId
                                      , "size"      .= uploadedMediaSize
                                      , "image"     .= uploadedMediaImage
                                      ]

-- | unicode code point indices, identifying the inclusive start and exclusive end of the displayable content of the Tweet.
data DisplayTextRange = DisplayTextRange
    { displayTextRangeStart :: Int
    , displayTextRangeEnd :: Int -- ^ exclusive
    } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance FromJSON DisplayTextRange where
    parseJSON v = do
        parseJSON v >>= \case
            [s, e] -> pure $ DisplayTextRange s e
            unexpected ->
                fail $ "parsing DisplayTextRange failed, expected [Int, Int], but got: " ++ show unexpected
instance ToJSON DisplayTextRange where
    toJSON (DisplayTextRange s e) = toJSON [s, e]
