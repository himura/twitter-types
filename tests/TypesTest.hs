{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Types

import Test.Framework.TH.Prime
import Test.Framework.Providers.HUnit
import Test.HUnit

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
  let ent = fromMaybe (Entities [] [] [] Nothing) $ statusEntities obj
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
  assert $ isJust media
  let Just m = media
  length m @?= 1
  let me = entityBody $ head m
  ueURL (meURL me) @?= "http://t.co/rJC5Pxsu"
  meMediaURLHttps me @?= "https://pbs.twimg.com/media/AZVLmp-CIAAbkyy.jpg"
  let sizes = meSizes me
  assert $ M.member "thumb" sizes
  assert $ M.member "large" sizes
