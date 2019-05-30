{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Fixtures where

import Language.Haskell.TH
import Data.Aeson
import Data.Attoparsec.ByteString
import qualified Data.ByteString as S
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

parseJSONValue :: S.ByteString -> Value
parseJSONValue = fromJust . maybeResult . parse json

fixturePath :: String
fixturePath = takeDirectory __FILE__ </> "fixtures"

loadFixture :: (S.ByteString -> a) -> String -> IO a
loadFixture conv filename = conv <$> S.readFile (fixturePath </> filename)

fixture :: (S.ByteString -> a) -> String -> a
fixture conv = unsafePerformIO . loadFixture conv

loadFixturesTH :: Name -> Q [Dec]
loadFixturesTH convFn = do
    files <- runIO $ filter (\fn -> takeExtension fn == ".json") <$> getDirectoryContents fixturePath
    concat <$> mapM genEachDefs files
  where
    genEachDefs filename = do
        let funN = mkName $ "fixture_" ++ dropExtension filename
        sigdef <- sigD funN (conT ''Value)
        bind <- valD (varP funN) (normalB [|fixture $(varE convFn) $(litE (stringL filename))|]) []
        return [ sigdef, bind ]
