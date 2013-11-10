{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Types

import Test.Framework.TH.Prime
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Shakespeare.Text

import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseEither)
import qualified Data.HashMap.Strict as M
import Data.Maybe

import Fixtures

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
case_parseStatus = withJSON statusJson $ \obj -> do
  statusId obj @?= 112652479837110273
  statusRetweetCount obj @?= Just 0
  (userScreenName . statusUser) obj @?= "imeoin"
  statusEntities obj @?= Nothing

case_parseStatusIncludeEntities :: Assertion
case_parseStatusIncludeEntities = withJSON statusEntityJson $ \obj -> do
  statusId obj @?= 112652479837110273
  statusRetweetCount obj @?= Just 0
  (userScreenName . statusUser) obj @?= "imeoin"
  let ent = fromMaybe (Entities [] [] [] []) $ statusEntities obj
  (map entityIndices . enHashTags) ent @?= [[32,42]]
  (hashTagText . entityBody . head . enHashTags) ent @?= "tcdisrupt"

case_parseErrorMsg :: Assertion
case_parseErrorMsg =
  case parseStatus errorMsgJson of
    Left str -> "Not authorized" @=? str
    Right _ -> assertFailure "errorMsgJson should be parsed as an error."
  where
    parseStatus :: Value -> Either String Status
    parseStatus = parseEither parseJSON

case_parseMediaEntity :: Assertion
case_parseMediaEntity = withJSON mediaEntityJson $ \obj -> do
  let entities = statusEntities obj
  assert $ isJust entities
  let Just ent = entities
      media = enMedia ent
  length media @?= 1
  let me = entityBody $ head media
  ueURL (meURL me) @?= "http://t.co/rJC5Pxsu"
  meMediaURLHttps me @?= "https://pbs.twimg.com/media/AZVLmp-CIAAbkyy.jpg"
  let sizes = meSizes me
  assert $ M.member "thumb" sizes
  assert $ M.member "large" sizes

case_parseEmptyEntity :: Assertion
case_parseEmptyEntity = withJSON (fj [st|{}|]) $ \entity -> do
    length (enHashTags entity) @?= 0
    length (enUserMentions entity) @?= 0
    length (enURLs entity) @?= 0
    length (enMedia entity) @?= 0

case_parseEntityHashTag :: Assertion
case_parseEntityHashTag = withJSON (fj [st|{"symbols":[],"urls":[{"indices":[32,52], "url":"http://t.co/IOwBrTZR", "display_url":"youtube.com/watch?v=oHg5SJ\u2026", "expanded_url":"http://www.youtube.com/watch?v=oHg5SJYRHA0"}],"user_mentions":[{"name":"Twitter API", "indices":[4,15], "screen_name":"twitterapi", "id":6253282, "id_str":"6253282"}],"hashtags":[{"indices":[32,36],"text":"lol"}]}|]) $ \entity -> do
    length (enHashTags entity) @?= 1
    length (enUserMentions entity) @?= 1
    length (enURLs entity) @?= 1
    length (enMedia entity) @?= 0

    let urlEntity = entityBody . head . enURLs $ entity
    ueURL urlEntity @?= "http://t.co/IOwBrTZR"
    ueExpanded urlEntity @?= "http://www.youtube.com/watch?v=oHg5SJYRHA0"
    ueDisplay urlEntity @?= "youtube.com/watch?v=oHg5SJ\x2026"

    let UserEntity mentionsUser = entityBody . head . enUserMentions $ entity
    userName mentionsUser @?= "Twitter API"
    userScreenName mentionsUser @?= "twitterapi"
    userId mentionsUser @?= 6253282

    let HashTagEntity hashtag = entityBody . head . enHashTags $ entity
    hashtag @?= "lol"