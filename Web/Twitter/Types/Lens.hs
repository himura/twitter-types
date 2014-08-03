{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RankNTypes, CPP, FlexibleInstances #-}

module Web.Twitter.Types.Lens
       ( TT.DateString
       , TT.UserId
       , TT.Friends
       , TT.URIString
       , TT.UserName
       , TT.StatusId
       , TT.LanguageCode
       , TT.StreamingAPI(..)
       , TT.Status
       , TT.SearchResult
       , TT.SearchStatus
       , TT.SearchMetadata
       , TT.RetweetedStatus
       , TT.DirectMessage
       , TT.EventTarget(..)
       , TT.Event
       , TT.Delete
       , TT.User
       , TT.List
       , TT.Entities
       , TT.EntityIndices
       , TT.Entity
       , TT.HashTagEntity
       , TT.UserEntity
       , TT.URLEntity
       , TT.MediaEntity
       , TT.MediaSize
       , TT.Coordinates
       , TT.Place
       , TT.BoundingBox

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

import Web.Twitter.Types.Lens.Types
import qualified Web.Twitter.Types as TT
import Data.Text (Text)
import Web.Twitter.Types.Lens.TH

makeLenses ''TT.Status
makeLenses ''TT.SearchResult
makeLenses ''TT.SearchStatus
makeLenses ''TT.SearchMetadata
makeLenses ''TT.RetweetedStatus
makeLenses ''TT.DirectMessage
makeLenses ''TT.Event
makeLenses ''TT.Delete
makeLenses ''TT.User
makeLenses ''TT.List
makeLenses ''TT.HashTagEntity
makeLenses ''TT.UserEntity
makeLenses ''TT.URLEntity
makeLenses ''TT.MediaEntity
makeLenses ''TT.MediaSize
makeLenses ''TT.Coordinates
makeLenses ''TT.Place
makeLenses ''TT.BoundingBox
makeLenses ''TT.Entities
makeLenses ''TT.Entity

class AsStatus s where
    status_id :: SimpleLens s TT.StatusId
    text :: SimpleLens s Text
    user :: SimpleLens s TT.User
    created_at :: SimpleLens s TT.DateString
    geolocation :: SimpleLens s (Maybe TT.Coordinates)

instance AsStatus TT.Status where
    status_id = statusId
    text = statusText
    user = statusUser
    created_at = statusCreatedAt
    geolocation = statusCoordinates

instance AsStatus TT.SearchStatus where
    status_id = searchStatusId
    text = searchStatusText
    user = searchStatusUser
    created_at = searchStatusCreatedAt
    geolocation = searchStatusCoordinates

instance AsStatus TT.RetweetedStatus where
    status_id = rsId
    text = rsText
    user = rsUser
    created_at = rsCreatedAt
    geolocation = rsCoordinates

instance AsStatus TT.DirectMessage where
    status_id = dmId
    text = dmText
    user = dmSender
    created_at = dmCreatedAt
    geolocation = dmCoordinates

class AsUser u where
    user_id :: SimpleLens u TT.UserId
    name :: SimpleLens u TT.UserName
    screen_name :: SimpleLens u Text

instance AsUser TT.User where
    user_id = userId
    name = userName
    screen_name = userScreenName

instance AsUser TT.UserEntity where
    user_id = userEntityUserId
    name = userEntityUserName
    screen_name = userEntityUserScreenName

instance AsUser (TT.Entity TT.UserEntity) where
    user_id = entityBody.userEntityUserId
    name = entityBody.userEntityUserName
    screen_name = entityBody.userEntityUserScreenName
