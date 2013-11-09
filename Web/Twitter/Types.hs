{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Web.Twitter.Types
       ( DateString
       , UserId
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
       , Entity(..)
       , HashTagEntity(..)
       , UserEntity(..)
       , URLEntity(..)
       , MediaEntity(..)
       , MediaSize(..)
       , checkError
       )
       where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Control.Applicative
import Control.Monad

type DateString   = String
type UserId       = Integer
type Friends      = [UserId]
type URIString    = ByteString
type UserName     = Text
type StatusId     = Integer
type LanguageCode = String

data StreamingAPI = SStatus Status
                  | SRetweetedStatus RetweetedStatus
                  | SEvent Event
                  | SDelete Delete
                  -- -- | SScrubGeo ScrubGeo
                  | SFriends Friends
                  | SUnknown Value
                  deriving (Show, Eq)

checkError :: Object -> Parser ()
checkError o = do
  err <- o .:? "error"
  case err of
    Just msg -> fail msg
    Nothing -> return ()

instance FromJSON StreamingAPI where
  parseJSON v@(Object o) =
    SRetweetedStatus <$> js <|>
    SStatus <$> js <|>
    SEvent <$> js <|>
    SDelete <$> js <|>
    SFriends <$> (o .: "friends") <|>
    return (SUnknown v)
    where
      js :: FromJSON a => Parser a
      js = parseJSON v
  parseJSON _ = mzero

data Status =
  Status
  { statusCreatedAt     :: DateString
  , statusId            :: StatusId
  , statusText          :: Text
  , statusSource        :: Text
  , statusTruncated     :: Bool
  , statusEntities      :: Maybe Entities
  , statusInReplyTo     :: Maybe StatusId
  , statusInReplyToUser :: Maybe UserId
  , statusFavorite      :: Maybe Bool
  , statusRetweetCount  :: Maybe Integer
  , statusUser          :: User
  } deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (Object o) = checkError o >>
    Status <$> o .:  "created_at"
           <*> o .:  "id"
           <*> o .:  "text"
           <*> o .:  "source"
           <*> o .:  "truncated"
           <*> o .:? "entities"
           <*> o .:? "in_reply_to_status_id"
           <*> o .:? "in_reply_to_user_id"
           <*> o .:? "favorited"
           <*> o .:? "retweet_count"
           <*> o .:  "user"
  parseJSON _ = mzero

data SearchResult body =
  SearchResult
  { searchResultStatuses :: body
  , searchResultSearchMetadata :: SearchMetadata
  } deriving (Show, Eq)

instance FromJSON body =>
         FromJSON (SearchResult body) where
  parseJSON (Object o) = checkError o >>
    SearchResult <$> o .:  "statuses"
                 <*> o .:  "search_metadata"
  parseJSON _ = mzero

data SearchStatus =
  SearchStatus
  { searchStatusCreatedAt     :: DateString
  , searchStatusId            :: StatusId
  , searchStatusText          :: Text
  , searchStatusSource        :: Text
  , searchStatusUser          :: User
  } deriving (Show, Eq)

instance FromJSON SearchStatus where
  parseJSON (Object o) = checkError o >>
    SearchStatus <$> o .:  "created_at"
                 <*> o .:  "id"
                 <*> o .:  "text"
                 <*> o .:  "source"
                 <*> o .:  "user"
  parseJSON _ = mzero

data SearchMetadata =
  SearchMetadata
  { searchMetadataMaxId         :: StatusId
  , searchMetadataSinceId       :: StatusId
  , searchMetadataRefreshUrl    :: URIString
  , searchMetadataNextResults   :: Maybe URIString
  , searchMetadataCount         :: Int
  , searchMetadataCompletedIn   :: Maybe Float
  , searchMetadataSinceIdStr    :: String
  , searchMetadataQuery         :: String
  , searchMetadataMaxIdStr      :: String
  } deriving (Show, Eq)

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
  parseJSON _ = mzero

data RetweetedStatus =
  RetweetedStatus
  { rsCreatedAt       :: DateString
  , rsId              :: StatusId
  , rsText            :: Text
  , rsSource          :: Text
  , rsTruncated       :: Bool
  , rsEntities        :: Maybe Entities
  , rsUser            :: User
  , rsRetweetedStatus :: Status
  } deriving (Show, Eq)

instance FromJSON RetweetedStatus where
  parseJSON (Object o) = checkError o >>
    RetweetedStatus <$> o .:  "created_at"
                    <*> o .:  "id"
                    <*> o .:  "text"
                    <*> o .:  "source"
                    <*> o .:  "truncated"
                    <*> o .:? "entities"
                    <*> o .:  "user"
                    <*> o .:  "retweeted_status"
  parseJSON _ = mzero

data DirectMessage =
  DirectMessage
  { dmCreatedAt          :: DateString
  , dmSenderScreenName   :: Text
  , dmSender             :: User
  , dmText               :: Text
  , dmRecipientScreeName :: Text
  , dmId                 :: StatusId
  , dmRecipient          :: User
  , dmRecipientId        :: UserId
  , dmSenderId           :: UserId
  } deriving (Show, Eq)

instance FromJSON DirectMessage where
  parseJSON (Object o) = checkError o >>
    DirectMessage <$> o .:  "created_at"
                  <*> o .:  "sender_screen_name"
                  <*> o .:  "sender"
                  <*> o .:  "text"
                  <*> o .:  "recipient_screen_name"
                  <*> o .:  "id"
                  <*> o .:  "recipient"
                  <*> o .:  "recipient_id"
                  <*> o .:  "sender_id"
  parseJSON _ = mzero

data EventType = Favorite | Unfavorite
               | ListCreated | ListUpdated | ListMemberAdded
               | UserUpdate | Block | Unblock | Follow
               deriving (Show, Eq)

data EventTarget = ETUser User | ETStatus Status | ETList List | ETUnknown Value
                 deriving (Show, Eq)

instance FromJSON EventTarget where
  parseJSON v@(Object o) = checkError o >>
    ETUser <$> parseJSON v <|>
    ETStatus <$> parseJSON v <|>
    ETList <$> parseJSON v <|>
    return (ETUnknown v)
  parseJSON _ = mzero

data Event =
  Event
  { evCreatedAt       :: DateString
  , evTargetObject    :: Maybe EventTarget
  , evEvent           :: Text
  , evTarget          :: EventTarget
  , evSource          :: EventTarget
  } deriving (Show, Eq)

instance FromJSON Event where
  parseJSON (Object o) = checkError o >>
    Event <$> o .:  "created_at"
          <*> o .:? "target_object"
          <*> o .:  "event"
          <*> o .:  "target"
          <*> o .:  "source"
  parseJSON _ = mzero

data Delete =
  Delete
  { delId  :: StatusId
  , delUserId :: UserId
  } deriving (Show, Eq)

instance FromJSON Delete where
  parseJSON (Object o) = checkError o >> do
    s <- o .: "delete" >>= (.: "status")
    Delete <$> s .: "id"
           <*> s .: "user_id"
  parseJSON _ = mzero

data User =
  User
  { userId              :: UserId
  , userName            :: UserName
  , userScreenName      :: Text
  , userDescription     :: Maybe Text
  , userLocation        :: Maybe Text
  , userProfileImageURL :: Maybe URIString
  , userURL             :: Maybe URIString
  , userProtected       :: Maybe Bool
  , userFollowers       :: Maybe Int
  , userFriends         :: Maybe Int
  , userTweets          :: Maybe Int
  , userLangCode        :: Maybe LanguageCode
  , userCreatedAt       :: Maybe DateString
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object o) = checkError o >>
    User <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "screen_name"
         <*> o .:? "description"
         <*> o .:? "location"
         <*> o .:? "profile_image_url"
         <*> o .:? "url"
         <*> o .:? "protected"
         <*> o .:? "followers_count"
         <*> o .:? "friends_count"
         <*> o .:? "statuses_count"
         <*> o .:? "lang"
         <*> o .:? "created_at"
  parseJSON _ = mzero

data List =
  List
  { listId :: Int
  , listName :: Text
  , listFullName :: Text
  , listMemberCount :: Int
  , listSubscriberCount :: Int
  , listMode :: Text
  , listUser :: User
  } deriving (Show, Eq)

instance FromJSON List where
  parseJSON (Object o) = checkError o >>
    List <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "full_name"
         <*> o .:  "member_count"
         <*> o .:  "subscriber_count"
         <*> o .:  "mode"
         <*> o .:  "user"
  parseJSON _ = mzero

data HashTagEntity =
  HashTagEntity
  { hashTagText :: Text -- ^ The Hashtag text
  } deriving (Show, Eq)

instance FromJSON HashTagEntity where
  parseJSON (Object o) =
    HashTagEntity <$> o .: "text"
  parseJSON _ = mzero

-- | The 'UserEntity' is just a wrapper around 'User' which is
--   a bit wasteful, and should probably be replaced by just
--   storing the id, name and screen name here.
data UserEntity = UserEntity User
                deriving (Show, Eq)

instance FromJSON UserEntity where
  parseJSON = (UserEntity <$>) . parseJSON

data URLEntity =
  URLEntity
  { ueURL      :: URIString -- ^ The URL that was extracted
  , ueExpanded :: URIString -- ^ The fully resolved URL (only for t.co links)
  , ueDisplay  :: Text    -- ^ Not a URL but a string to display instead of the URL (only for t.co links)
  } deriving (Show, Eq)

instance FromJSON URLEntity where
  parseJSON (Object o) =
    URLEntity <$> o .:  "url"
              <*> o .:  "expanded_url"
              <*> o .:  "display_url"
  parseJSON _ = mzero

data MediaEntity =
  MediaEntity
  { meType :: Text
  , meId :: StatusId
  , meSizes :: HashMap Text MediaSize
  , meMediaURL :: URIString
  , meMediaURLHttps :: URIString
  , meURL :: URLEntity
  } deriving (Show, Eq)

instance FromJSON MediaEntity where
  parseJSON v@(Object o) =
    MediaEntity <$> o .: "type"
                <*> o .: "id"
                <*> o .: "sizes"
                <*> o .: "media_url"
                <*> o .: "media_url_https"
                <*> parseJSON v
  parseJSON _ = mzero

data MediaSize =
  MediaSize
  { msWidth :: Int
  , msHeight :: Int
  , msResize :: Text
  } deriving (Show, Eq)

instance FromJSON MediaSize where
  parseJSON (Object o) =
    MediaSize <$> o .: "w"
              <*> o .: "h"
              <*> o .: "resize"
  parseJSON _ = mzero

-- | Entity handling.
data Entities =
  Entities
  { enHashTags     :: [Entity HashTagEntity]
  , enUserMentions :: [Entity UserEntity]
  , enURLs         :: [Entity URLEntity]
  , enMedia        :: Maybe [Entity MediaEntity]
  } deriving (Show, Eq)

instance FromJSON Entities where
  parseJSON (Object o) =
    Entities <$> o .:  "hashtags"
             <*> o .:  "user_mentions"
             <*> o .:  "urls"
             <*> o .:? "media"
  parseJSON _ = mzero

-- | The character positions the Entity was extracted from
--
--   This is experimental implementation.
--   This may be replaced by more definite types.
type EntityIndices = [Int]

data Entity a =
  Entity
  { entityBody    :: a             -- ^ The detail information of the specific entity types (HashTag, URL, User)
  , entityIndices :: EntityIndices -- ^ The character positions the Entity was extracted from
  } deriving (Show, Eq)

instance FromJSON a => FromJSON (Entity a) where
  parseJSON v@(Object o) =
    Entity <$> parseJSON v
           <*> o .: "indices"
  parseJSON _ = mzero
