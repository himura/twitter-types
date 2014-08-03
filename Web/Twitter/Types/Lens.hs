{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RankNTypes, CPP, FlexibleInstances #-}

module Web.Twitter.Types.Lens
       ( DateString
       , UserId
       , Friends
       , URIString
       , UserName
       , StatusId
       , LanguageCode
       , StreamingAPI(..)
       , Status
       , SearchResult
       , SearchStatus
       , SearchMetadata
       , RetweetedStatus
       , DirectMessage
       , EventTarget(..)
       , Event
       , Delete
       , User
       , List
       , Entities
       , EntityIndices
       , Entity
       , HashTagEntity
       , UserEntity
       , URLEntity
       , MediaEntity
       , MediaSize
       , Coordinates
       , Place
       , BoundingBox

       , statusCreatedAt
       , statusId
       , statusText
       , statusSource
       , statusTruncated
       , statusEntities
       , statusExtendedEntities
       , statusInReplyTo
       , statusInReplyToUser
       , statusFavorite
       , statusRetweetCount
       , statusRetweet
       , statusUser
       , statusPlace
       , statusFavoriteCount
       , statusLang
       , statusPossiblySensitive
       , statusCoordinates

       , searchResultStatuses
       , searchResultSearchMetadata

       , searchStatusCreatedAt
       , searchStatusId
       , searchStatusText
       , searchStatusSource
       , searchStatusUser
       , searchStatusCoordinates

       , searchMetadataMaxId
       , searchMetadataSinceId
       , searchMetadataRefreshUrl
       , searchMetadataNextResults
       , searchMetadataCount
       , searchMetadataCompletedIn
       , searchMetadataSinceIdStr
       , searchMetadataQuery
       , searchMetadataMaxIdStr

       , rsCreatedAt
       , rsId
       , rsText
       , rsSource
       , rsTruncated
       , rsEntities
       , rsUser
       , rsRetweetedStatus
       , rsCoordinates

       , dmCreatedAt
       , dmSenderScreenName
       , dmSender
       , dmText
       , dmRecipientScreeName
       , dmId
       , dmRecipient
       , dmRecipientId
       , dmSenderId
       , dmCoordinates

       , evCreatedAt
       , evTargetObject
       , evEvent
       , evTarget
       , evSource

       , delId
       , delUserId

       , userId
       , userName
       , userScreenName
       , userDescription
       , userLocation
       , userProfileImageURL
       , userURL
       , userProtected
       , userFollowers
       , userFriends
       , userTweets
       , userLangCode
       , userCreatedAt

       , listId
       , listName
       , listFullName
       , listMemberCount
       , listSubscriberCount
       , listMode
       , listUser

       , hashTagText

       , userEntityUserId
       , userEntityUserName
       , userEntityUserScreenName

       , ueURL
       , ueExpanded
       , ueDisplay

       , meType
       , meId
       , meSizes
       , meMediaURL
       , meMediaURLHttps
       , meURL

       , msWidth
       , msHeight
       , msResize

       , coordinates
       , coordinatesType

       , placeAttributes
       , placeBoundingBox
       , placeCountry
       , placeCountryCode
       , placeFullName
       , placeId
       , placeName
       , placeType
       , placeUrl

       , boundingBoxCoordinates
       , boundingBoxType

       , enHashTags
       , enUserMentions
       , enURLs
       , enMedia

       , entityBody
       , entityIndices

       , AsStatus(..)
       , AsUser(..)
       )
       where

import Web.Twitter.Types
       ( DateString
       , UserId
       , Friends
       , URIString
       , UserName
       , StatusId
       , LanguageCode
       , StreamingAPI
       , Status
       , SearchResult
       , SearchStatus
       , SearchMetadata
       , RetweetedStatus
       , DirectMessage
       , EventTarget
       , Event
       , Delete
       , User
       , List
       , Entities
       , EntityIndices
       , Entity
       , HashTagEntity
       , UserEntity
       , URLEntity
       , MediaEntity
       , MediaSize
       , Coordinates
       , Place
       , BoundingBox
       )
import Data.Text (Text)
import Web.Twitter.Types.TH

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type SimpleLens s a = Lens s s a a

makeLenses ''Status
makeLenses ''SearchResult
makeLenses ''SearchStatus
makeLenses ''SearchMetadata
makeLenses ''RetweetedStatus
makeLenses ''DirectMessage
makeLenses ''Event
makeLenses ''Delete
makeLenses ''User
makeLenses ''List
makeLenses ''HashTagEntity
makeLenses ''UserEntity
makeLenses ''URLEntity
makeLenses ''MediaEntity
makeLenses ''MediaSize
makeLenses ''Coordinates
makeLenses ''Place
makeLenses ''BoundingBox
makeLenses ''Entities
makeLenses ''Entity

class AsStatus s where
    status_id :: SimpleLens s StatusId
    text :: SimpleLens s Text
    user :: SimpleLens s User
    created_at :: SimpleLens s DateString
    geolocation :: SimpleLens s (Maybe Coordinates)

instance AsStatus Status where
    status_id = statusId
    text = statusText
    user = statusUser
    created_at = statusCreatedAt
    geolocation = statusCoordinates

instance AsStatus SearchStatus where
    status_id = searchStatusId
    text = searchStatusText
    user = searchStatusUser
    created_at = searchStatusCreatedAt
    geolocation = searchStatusCoordinates

instance AsStatus RetweetedStatus where
    status_id = rsId
    text = rsText
    user = rsUser
    created_at = rsCreatedAt
    geolocation = rsCoordinates

instance AsStatus DirectMessage where
    status_id = dmId
    text = dmText
    user = dmSender
    created_at = dmCreatedAt
    geolocation = dmCoordinates

class AsUser u where
    user_id :: SimpleLens u UserId
    name :: SimpleLens u UserName
    screen_name :: SimpleLens u Text

instance AsUser User where
    user_id = userId
    name = userName
    screen_name = userScreenName

instance AsUser UserEntity where
    user_id = userEntityUserId
    name = userEntityUserName
    screen_name = userEntityUserScreenName

instance AsUser (Entity UserEntity) where
    user_id = entityBody.userEntityUserId
    name = entityBody.userEntityUserName
    screen_name = entityBody.userEntityUserScreenName
