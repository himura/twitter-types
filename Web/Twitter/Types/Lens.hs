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
       , Place
       , BoundingBox

       , statusCreatedAt
       , statusId
       , statusText
       , statusSource
       , statusTruncated
       , statusEntities
       , statusInReplyTo
       , statusInReplyToUser
       , statusFavorite
       , statusRetweetCount
       , statusRetweet
       , statusUser
       , statusPlace

       , searchResultStatuses
       , searchResultSearchMetadata

       , searchStatusCreatedAt
       , searchStatusId
       , searchStatusText
       , searchStatusSource
       , searchStatusUser

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

       , dmCreatedAt
       , dmSenderScreenName
       , dmSender
       , dmText
       , dmRecipientScreeName
       , dmId
       , dmRecipient
       , dmRecipientId
       , dmSenderId

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

import Data.Functor
import qualified Web.Twitter.Types as TT
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
       , Place
       , BoundingBox
       )
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

#define SIMPLE_LENS(name, s, a) \
name :: Lens' (s) (a);\
name f record = (\newVal -> record { TT.name = newVal }) <$> (f (TT.name record));\
{-# INLINE name #-}

#define TYPECHANGE_LENS(name, s) \
name :: Lens ((s) a) ((s) b) (a) (b);\
name f record = (\newVal -> record { TT.name = newVal }) <$> (f (TT.name record))

SIMPLE_LENS(statusCreatedAt           , Status,  DateString                   )
SIMPLE_LENS(statusId                  , Status,  StatusId                     )
SIMPLE_LENS(statusText                , Status,  Text                         )
SIMPLE_LENS(statusSource              , Status,  Text                         )
SIMPLE_LENS(statusTruncated           , Status,  Bool                         )
SIMPLE_LENS(statusEntities            , Status,  Maybe Entities               )
SIMPLE_LENS(statusInReplyTo           , Status,  Maybe StatusId               )
SIMPLE_LENS(statusInReplyToUser       , Status,  Maybe UserId                 )
SIMPLE_LENS(statusFavorite            , Status,  Maybe Bool                   )
SIMPLE_LENS(statusRetweetCount        , Status,  Maybe Integer                )
SIMPLE_LENS(statusRetweet             , Status,  Maybe Status                 )
SIMPLE_LENS(statusUser                , Status,  User                         )
SIMPLE_LENS(statusPlace               , Status,  Maybe Place                  )

TYPECHANGE_LENS(searchResultStatuses  , SearchResult                          )
SIMPLE_LENS(searchResultSearchMetadata, SearchResult body,  SearchMetadata    )

SIMPLE_LENS(searchStatusCreatedAt     , SearchStatus,  DateString             )
SIMPLE_LENS(searchStatusId            , SearchStatus,  StatusId               )
SIMPLE_LENS(searchStatusText          , SearchStatus,  Text                   )
SIMPLE_LENS(searchStatusSource        , SearchStatus,  Text                   )
SIMPLE_LENS(searchStatusUser          , SearchStatus,  User                   )

SIMPLE_LENS(searchMetadataMaxId       , SearchMetadata,  StatusId             )
SIMPLE_LENS(searchMetadataSinceId     , SearchMetadata,  StatusId             )
SIMPLE_LENS(searchMetadataRefreshUrl  , SearchMetadata,  URIString            )
SIMPLE_LENS(searchMetadataNextResults , SearchMetadata,  Maybe URIString      )
SIMPLE_LENS(searchMetadataCount       , SearchMetadata,  Int                  )
SIMPLE_LENS(searchMetadataCompletedIn , SearchMetadata,  Maybe Float          )
SIMPLE_LENS(searchMetadataSinceIdStr  , SearchMetadata,  String               )
SIMPLE_LENS(searchMetadataQuery       , SearchMetadata,  String               )
SIMPLE_LENS(searchMetadataMaxIdStr    , SearchMetadata,  String               )

SIMPLE_LENS(rsCreatedAt               , RetweetedStatus,  DateString          )
SIMPLE_LENS(rsId                      , RetweetedStatus,  StatusId            )
SIMPLE_LENS(rsText                    , RetweetedStatus,  Text                )
SIMPLE_LENS(rsSource                  , RetweetedStatus,  Text                )
SIMPLE_LENS(rsTruncated               , RetweetedStatus,  Bool                )
SIMPLE_LENS(rsEntities                , RetweetedStatus,  Maybe Entities      )
SIMPLE_LENS(rsUser                    , RetweetedStatus,  User                )
SIMPLE_LENS(rsRetweetedStatus         , RetweetedStatus,  Status              )

SIMPLE_LENS(dmCreatedAt               , DirectMessage,  DateString            )
SIMPLE_LENS(dmSenderScreenName        , DirectMessage,  Text                  )
SIMPLE_LENS(dmSender                  , DirectMessage,  User                  )
SIMPLE_LENS(dmText                    , DirectMessage,  Text                  )
SIMPLE_LENS(dmRecipientScreeName      , DirectMessage,  Text                  )
SIMPLE_LENS(dmId                      , DirectMessage,  StatusId              )
SIMPLE_LENS(dmRecipient               , DirectMessage,  User                  )
SIMPLE_LENS(dmRecipientId             , DirectMessage,  UserId                )
SIMPLE_LENS(dmSenderId                , DirectMessage,  UserId                )

SIMPLE_LENS(evCreatedAt               , Event,  DateString                    )
SIMPLE_LENS(evTargetObject            , Event,  Maybe EventTarget             )
SIMPLE_LENS(evEvent                   , Event,  Text                          )
SIMPLE_LENS(evTarget                  , Event,  EventTarget                   )
SIMPLE_LENS(evSource                  , Event,  EventTarget                   )

SIMPLE_LENS(delId                     , Delete,  StatusId                     )
SIMPLE_LENS(delUserId                 , Delete,  UserId                       )

SIMPLE_LENS(userId                    , User,  UserId                         )
SIMPLE_LENS(userName                  , User,  UserName                       )
SIMPLE_LENS(userScreenName            , User,  Text                           )
SIMPLE_LENS(userDescription           , User,  Maybe Text                     )
SIMPLE_LENS(userLocation              , User,  Maybe Text                     )
SIMPLE_LENS(userProfileImageURL       , User,  Maybe URIString                )
SIMPLE_LENS(userURL                   , User,  Maybe URIString                )
SIMPLE_LENS(userProtected             , User,  Maybe Bool                     )
SIMPLE_LENS(userFollowers             , User,  Maybe Int                      )
SIMPLE_LENS(userFriends               , User,  Maybe Int                      )
SIMPLE_LENS(userTweets                , User,  Maybe Int                      )
SIMPLE_LENS(userLangCode              , User,  Maybe LanguageCode             )
SIMPLE_LENS(userCreatedAt             , User,  Maybe DateString               )

SIMPLE_LENS(listId                    , List,  Int                            )
SIMPLE_LENS(listName                  , List,  Text                           )
SIMPLE_LENS(listFullName              , List,  Text                           )
SIMPLE_LENS(listMemberCount           , List,  Int                            )
SIMPLE_LENS(listSubscriberCount       , List,  Int                            )
SIMPLE_LENS(listMode                  , List,  Text                           )
SIMPLE_LENS(listUser                  , List,  User                           )

SIMPLE_LENS(hashTagText               , HashTagEntity,  Text                  )

SIMPLE_LENS(userEntityUserId          , UserEntity,  UserId                   )
SIMPLE_LENS(userEntityUserName        , UserEntity,  UserName                 )
SIMPLE_LENS(userEntityUserScreenName  , UserEntity,  Text                     )

SIMPLE_LENS(ueURL                     , URLEntity,  URIString                 )
SIMPLE_LENS(ueExpanded                , URLEntity,  URIString                 )
SIMPLE_LENS(ueDisplay                 , URLEntity,  Text                      )

SIMPLE_LENS(meType                    , MediaEntity,  Text                    )
SIMPLE_LENS(meId                      , MediaEntity,  StatusId                )
SIMPLE_LENS(meSizes                   , MediaEntity,  HashMap Text MediaSize  )
SIMPLE_LENS(meMediaURL                , MediaEntity,  URIString               )
SIMPLE_LENS(meMediaURLHttps           , MediaEntity,  URIString               )
SIMPLE_LENS(meURL                     , MediaEntity,  URLEntity               )

SIMPLE_LENS(msWidth                   , MediaSize,  Int                       )
SIMPLE_LENS(msHeight                  , MediaSize,  Int                       )
SIMPLE_LENS(msResize                  , MediaSize,  Text                      )

SIMPLE_LENS(placeAttributes           , Place,  HashMap Text Text             )
SIMPLE_LENS(placeBoundingBox          , Place,  BoundingBox                   )
SIMPLE_LENS(placeCountry              , Place,  Text                          )
SIMPLE_LENS(placeCountryCode          , Place,  Text                          )
SIMPLE_LENS(placeFullName             , Place,  Text                          )
SIMPLE_LENS(placeId                   , Place,  Text                          )
SIMPLE_LENS(placeName                 , Place,  Text                          )
SIMPLE_LENS(placeType                 , Place,  Text                          )
SIMPLE_LENS(placeUrl                  , Place,  Text                          )

SIMPLE_LENS(boundingBoxCoordinates    , BoundingBox,  [[[Double]]]            )
SIMPLE_LENS(boundingBoxType           , BoundingBox,  Text                    )

SIMPLE_LENS(enHashTags                , Entities,  [Entity HashTagEntity]     )
SIMPLE_LENS(enUserMentions            , Entities,  [Entity UserEntity]        )
SIMPLE_LENS(enURLs                    , Entities,  [Entity URLEntity]         )
SIMPLE_LENS(enMedia                   , Entities,  [Entity MediaEntity]       )

TYPECHANGE_LENS(entityBody            , Entity                                )
SIMPLE_LENS(entityIndices             , Entity a,  EntityIndices              )

class AsStatus s where
    status_id :: Lens' s StatusId
    text :: Lens' s Text
    user :: Lens' s User
    created_at :: Lens' s DateString

instance AsStatus Status where
    status_id = statusId
    text = statusText
    user = statusUser
    created_at = statusCreatedAt

instance AsStatus SearchStatus where
    status_id = searchStatusId
    text = searchStatusText
    user = searchStatusUser
    created_at = searchStatusCreatedAt

instance AsStatus RetweetedStatus where
    status_id = rsId
    text = rsText
    user = rsUser
    created_at = rsCreatedAt

instance AsStatus DirectMessage where
    status_id = dmId
    text = dmText
    user = dmSender
    created_at = dmCreatedAt

class AsUser u where
    user_id :: Lens' u UserId
    name :: Lens' u UserName
    screen_name :: Lens' u Text

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
