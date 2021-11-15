{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import System.Environment
import Text.Shakespeare.Text (st)
import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.XPath

data Field = Field
    { fieldName :: String
    , fieldHaskellName :: Maybe String
    , fieldMappedType :: String
    , fieldNullable :: Bool
    }
    deriving (Show)

parseXML :: String -> IOStateArrow s b XmlTree
parseXML =
    readString
        [ withValidate no
        , withWarnings no
        ]

getXPathText :: ArrowXml a => String -> a XmlTree String
getXPathText xpath = getXPathTrees xpath //> getText

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

parseXMLField :: ArrowXml a => a XmlTree Field
parseXMLField = proc tree -> do
    objs <- getXPathTrees "//field" -< tree
    deep eachField -< objs

eachField :: ArrowXml a => a XmlTree Field
eachField =
    atTag "field" >>> proc tree -> do
        field <- getXPathText "//name[1]" -< tree
        haskellName <- (getXPathText "//haskell_name[1]" >>> arr Just) `orElse` constA Nothing -< tree
        mappedType <- getXPathText "//mapped_type[1]" -< tree
        nullable <- getAttrValue "nullable" -< tree
        returnA -< Field field haskellName mappedType (nullable == "true")

snakeToLowerCamel :: String -> String
snakeToLowerCamel [] = []
snakeToLowerCamel ('_' : []) = []
snakeToLowerCamel ('_' : x : xs) = toUpper x : snakeToLowerCamel xs
snakeToLowerCamel str = f ++ snakeToLowerCamel next
  where
    (f, next) = span (/= '_') str

snakeToUpperCamel :: String -> String
snakeToUpperCamel = upcase . snakeToLowerCamel
  where
    upcase [] = []
    upcase (x : xs) = toUpper x : xs

genTypeField :: String -> String -> Field -> T.Text
genTypeField prefix commaOrBrace Field {..} =
    [st|    #{commaOrBrace} #{prefix}#{name} :: #{mb}#{fieldMappedType}|]
  where
    name = snakeToUpperCamel $ fromMaybe fieldName fieldHaskellName
    mb :: String
    mb = if fieldNullable then "Maybe " else ""

genFromJSON :: String -> Field -> T.Text
genFromJSON prefix Field {..} =
    [st|        #{prefix} o .:#{mb} "#{fieldName}"|]
  where
    mb :: String
    mb = if fieldNullable then "?" else " "

main :: IO ()
main = do
    (prefix : input : []) <- getArgs
    content <- L.readFile input
    [tree] <- runX $ parseXML (L8.unpack content)
    let fields = runLA parseXMLField tree
        typeName = snakeToUpperCamel prefix

    T.putStrLn . T.intercalate "\n" $
        [ [st|data #{typeName} = #{typeName}|]
        , T.intercalate "\n" $ map (\(field, cb) -> genTypeField prefix cb field) (zip fields ("{" : repeat ","))
        , "    }"
        , ""
        , [st|
instance FromJSON #{typeName} where
    parseJSON (Object o) =|]
        , T.intercalate "\n" $ map (\(field, cb) -> genFromJSON cb field) (zip fields ((typeName ++ " <$>") : repeat (take (length typeName) (repeat ' ') ++ " <*>")))
        , "    parseJSON _ = mzero"
        ]
