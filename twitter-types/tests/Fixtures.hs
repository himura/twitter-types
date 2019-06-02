{-# LANGUAGE CPP #-}

module Fixtures where

import Data.Aeson
import Data.ByteString as S
import System.FilePath
import Test.Tasty.HUnit

fixturePath :: FilePath -> FilePath
fixturePath filename = takeDirectory __FILE__ </> "fixtures" </> filename

readFixtureFile :: FilePath -> IO ByteString
readFixtureFile = S.readFile . fixturePath

withFixtureJSON :: FromJSON a => FilePath -> (a -> Assertion) -> Assertion
withFixtureJSON filename assertions = do
    body <- readFixtureFile filename
    withJSON body assertions

withJSON :: FromJSON a => ByteString -> (a -> Assertion) -> Assertion
withJSON body assertions = do
    case eitherDecodeStrict' body of
        Left err -> assertFailure $ err
        Right result -> assertions result
