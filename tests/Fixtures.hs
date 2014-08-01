{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Fixtures where

import Language.Haskell.TH
import Data.Aeson
import Data.Attoparsec.ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative

parseJSONValue :: T.Text -> Value
parseJSONValue = fromJust . maybeResult . parse json . T.encodeUtf8

fixturePath :: String
fixturePath = unsafePerformIO $ do
    fromMaybe defaultPath <$> lookupEnv "TWITTER_FIXTURE_PATH"
  where
    defaultPath = takeDirectory __FILE__ </> "fixtures"

loadFixture :: (T.Text -> a) -> String -> IO a
loadFixture conv filename = conv <$> T.readFile (fixturePath </> filename)

fixture :: (T.Text -> a) -> String -> a
fixture conv = unsafePerformIO . loadFixture conv

loadFixtureTH :: Name -> Q [Dec]
loadFixtureTH convFn = do
    files <- runIO $ filter (\fn -> takeExtension fn == ".json") <$> getDirectoryContents fixturePath
    concat <$> mapM genEachDefs files
  where
    genEachDefs filename = do
        let funN = mkName $ "fixture_" ++ dropExtension filename
        sigdef <- sigD funN (conT ''Value)
        bind <- valD (varP funN) (normalB [|fixture $(varE convFn) $(litE (stringL filename))|]) []
        return [ sigdef, bind ]
