{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Twitter.Types.Lens
       (
       -- * Type classes
         AsStatus(..)
       , AsUser(..)
       , HasCreatedAt(..)
       , AsImageSize(..)

       -- * 'TT.Status'
       , TT.Status
       , statusContributors
       , statusCoordinates
       , statusCreatedAt
       , statusCurrentUserRetweet
       , statusEntities
       , statusExtendedEntities
       , statusFavoriteCount
       , statusFavorited
       , statusFilterLevel
       , statusId
       , statusInReplyToScreenName
       , statusInReplyToStatusId
       , statusInReplyToUserId
       , statusLang
       , statusPlace
       , statusPossiblySensitive
       , statusScopes
       , statusRetweetCount
       , statusRetweeted
       , statusRetweetedStatus
       , statusSource
       , statusText
       , statusTruncated
       , statusUser
       , statusWithheldCopyright
       , statusWithheldInCountries
       , statusWithheldScope

       -- * 'TT.SearchResult'
       , TT.SearchResult
       , searchResultStatuses
       , searchResultSearchMetadata

       -- * 'TT.SearchStatus'
       , TT.SearchStatus
       , searchStatusCreatedAt
       , searchStatusId
       , searchStatusText
       , searchStatusSource
       , searchStatusUser
       , searchStatusCoordinates

       -- * 'TT.SearchMetadata'
       , TT.SearchMetadata
       , searchMetadataMaxId
       , searchMetadataSinceId
       , searchMetadataRefreshURL
       , searchMetadataNextResults
       , searchMetadataCount
       , searchMetadataCompletedIn
       , searchMetadataSinceIdStr
       , searchMetadataQuery
       , searchMetadataMaxIdStr

       -- * 'TT.RetweetedStatus'
       , TT.RetweetedStatus
       , rsCreatedAt
       , rsId
       , rsText
       , rsSource
       , rsTruncated
       , rsEntities
       , rsUser
       , rsRetweetedStatus
       , rsCoordinates

       -- * 'TT.DirectMessage'
       , TT.DirectMessage
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

       -- * 'TT.Event'
       , TT.Event
       , evCreatedAt
       , evTargetObject
       , evEvent
       , evTarget
       , evSource

       -- * 'TT.Delete'
       , TT.Delete
       , delId
       , delUserId

       -- * 'TT.User'
       , TT.User
       , userContributorsEnabled
       , userCreatedAt
       , userDefaultProfile
       , userDefaultProfileImage
       , userDescription
       , userFavoritesCount
       , userFollowRequestSent
       , userFollowing
       , userFollowersCount
       , userFriendsCount
       , userGeoEnabled
       , userId
       , userIsTranslator
       , userLang
       , userListedCount
       , userLocation
       , userName
       , userNotifications
       , userProfileBackgroundColor
       , userProfileBackgroundImageURL
       , userProfileBackgroundImageURLHttps
       , userProfileBackgroundTile
       , userProfileBannerURL
       , userProfileImageURL
       , userProfileImageURLHttps
       , userProfileLinkColor
       , userProfileSidebarBorderColor
       , userProfileSidebarFillColor
       , userProfileTextColor
       , userProfileUseBackgroundImage
       , userProtected
       , userScreenName
       , userShowAllInlineMedia
       , userStatusesCount
       , userTimeZone
       , userURL
       , userUtcOffset
       , userVerified
       , userWithheldInCountries
       , userWithheldScope

       -- * 'TT.List'
       , TT.List
       , listId
       , listName
       , listFullName
       , listMemberCount
       , listSubscriberCount
       , listMode
       , listUser

       -- * 'TT.Entities'
       , TT.Entities
       , enHashTags
       , enUserMentions
       , enURLs
       , enMedia

       -- * 'TT.Entity'
       , TT.Entity
       , entityBody
       , entityIndices

       -- * 'TT.HashTagEntity'
       , TT.HashTagEntity
       , hashTagText

       -- * 'TT.UserEntity'
       , TT.UserEntity
       , userEntityUserId
       , userEntityUserName
       , userEntityUserScreenName

       -- * 'TT.URLEntity'
       , TT.URLEntity
       , ueURL
       , ueExpanded
       , ueDisplay

       -- * 'TT.MediaEntity'
       , TT.MediaEntity
       , meType
       , meId
       , meSizes
       , meMediaURL
       , meMediaURLHttps
       , meURL

       -- * 'TT.MediaSize'
       , TT.MediaSize
       , msWidth
       , msHeight
       , msResize

       -- * 'TT.Coordinates'
       , TT.Coordinates
       , coordinates
       , coordinatesType

       -- * 'TT.Place'
       , TT.Place
       , placeAttributes
       , placeBoundingBox
       , placeCountry
       , placeCountryCode
       , placeFullName
       , placeId
       , placeName
       , placeType
       , placeURL

       -- * 'TT.BoundingBox'
       , TT.BoundingBox
       , boundingBoxCoordinates
       , boundingBoxType

       -- * 'TT.Contributor'
       , TT.Contributor
       , contributorId
       , contributorScreenName

       -- * 'TT.UploadedMedia'
       , TT.UploadedMedia
       , uploadedMediaId
       , uploadedMediaSize
       , uploadedMediaImage

       -- * 'TT.ImageSizeType'
       , TT.ImageSizeType
       , imageSizeTypeWidth
       , imageSizeTypeHeight
       , imageSizeTypeType

       -- * Type aliases and sum types
       , TT.DateString
       , TT.UserId
       , TT.Friends
       , TT.URIString
       , TT.UserName
       , TT.StatusId
       , TT.LanguageCode
       , TT.StreamingAPI(..)
       , TT.EventTarget(..)
       , TT.EntityIndices

       -- * 'TT.StreamingAPI'
       , _SStatus
       , _SRetweetedStatus
       , _SEvent
       , _SDelete
       , _SFriends
       , _SUnknown

       -- * 'TT.EventTarget'
       , _ETUser
       , _ETStatus
       , _ETList
       , _ETUnknown
       )
       where

import Control.Lens hiding (makeLenses)
import Data.Text (Text)
import qualified Web.Twitter.Types as TT
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
makeLenses ''TT.Entities
makeLenses ''TT.Entity
makeLenses ''TT.HashTagEntity
makeLenses ''TT.UserEntity
makeLenses ''TT.URLEntity
makeLenses ''TT.MediaEntity
makeLenses ''TT.MediaSize
makeLenses ''TT.Coordinates
makeLenses ''TT.Place
makeLenses ''TT.BoundingBox
makeLenses ''TT.Contributor
makeLenses ''TT.ImageSizeType
makeLenses ''TT.UploadedMedia

class AsStatus s where
    status_id :: Lens' s TT.StatusId
    text :: Lens' s Text
    user :: Lens' s TT.User
    geolocation :: Lens' s (Maybe TT.Coordinates)

instance AsStatus TT.Status where
    status_id = statusId
    text = statusText
    user = statusUser
    geolocation = statusCoordinates

instance AsStatus TT.SearchStatus where
    status_id = searchStatusId
    text = searchStatusText
    user = searchStatusUser
    geolocation = searchStatusCoordinates

instance AsStatus TT.RetweetedStatus where
    status_id = rsId
    text = rsText
    user = rsUser
    geolocation = rsCoordinates

instance AsStatus TT.DirectMessage where
    status_id = dmId
    text = dmText
    user = dmSender
    geolocation = dmCoordinates

class AsUser u where
    user_id :: Lens' u TT.UserId
    name :: Lens' u TT.UserName
    screen_name :: Lens' u Text

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

class HasCreatedAt a where
    created_at :: Lens' a TT.DateString
instance HasCreatedAt TT.Status where
    created_at = statusCreatedAt
instance HasCreatedAt TT.SearchStatus where
    created_at = searchStatusCreatedAt
instance HasCreatedAt TT.RetweetedStatus where
    created_at = rsCreatedAt
instance HasCreatedAt TT.DirectMessage where
    created_at = dmCreatedAt
instance HasCreatedAt TT.User where
    created_at = userCreatedAt

class AsImageSize a where
    width :: Lens' a Int
    height :: Lens' a Int
instance AsImageSize TT.MediaSize where
    width = msWidth
    height = msHeight
instance AsImageSize TT.ImageSizeType where
    width = imageSizeTypeWidth
    height = imageSizeTypeHeight

makePrisms ''TT.StreamingAPI
makePrisms ''TT.EventTarget
