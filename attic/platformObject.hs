{-# LANGUAGE Arrows #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Network.HTTP.Conduit
import System.Environment
import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.XPath

data Field = Field
    { fieldName :: String
    , fieldType :: String
    , fieldDescription :: String
    , fieldNullable :: Bool
    } deriving Show

parseHTML :: String -> IOStateArrow s b XmlTree
parseHTML = readString [ withValidate no
                       , withParseHTML yes
                       , withWarnings no
                       ]

getXPathText :: ArrowXml a => String -> a XmlTree String
getXPathText xpath = getXPathTrees xpath //> getText

cleanStr :: String -> String
cleanStr = reverse . dropWhile isSpace . reverse . dropWhile isSpace

getHTMLTree
  :: Request
  -> Manager
  -> IO [XmlTree]
getHTMLTree req mgr = do
  res <- httpLbs req mgr
  let body = L8.unpack . responseBody $ res
  runX $ parseHTML body

platformObjects :: ArrowXml a => a XmlTree Field
platformObjects = proc tree -> do
    objs <- getXPathTrees "//table[@class='platform-object'][1]" -< tree
    deep eachObj -< objs

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

eachObj :: ArrowXml a => a XmlTree Field
eachObj = atTag "tr" >>> proc tree -> do
    field <- getXPathText "//th[1]" -< tree
    typ <- getXPathText "//td[1]" -< tree
    desc <- listA (getXPathText "//td[2]") -< tree
    nullable <- (getXPathText "//td[2]/em[1]" >>> arr ((== "Nullable") . cleanStr)) `orElse` constA False -< tree
    returnA -< Field (cleanStr field) (cleanStr typ) (cleanStr $ concat desc) nullable

fieldElem :: ArrowXml a => a Field XmlTree
fieldElem =
    mkelem "field" [mkAttr (mkQName "" "nullable" "") (this >>> arr fieldNullable >>> arr (map toLower . show) >>> mkText)]
        [ selem "name" [this >>> arr fieldName >>> mkText]
        , selem "type" [this >>> arr fieldType >>> mkText]
        , selem "mapped_type" [this >>> arr (mappedType . fieldType) >>> mkText]
        , selem "description" [this >>> arr fieldDescription >>> mkText]
        ]

mappedType :: String -> String
mappedType "Boolean" = "Bool"
mappedType "String" = "Text"
mappedType a = a

main :: IO ()
main = do
    [input, output] <- getArgs
    content <- L.readFile input
    [tree] <- runX $ parseHTML (L8.unpack content)
    let fields = runLA platformObjects tree
    _ <- runX $
        root [] [selem "platform-object" [constL fields >>> fieldElem]]
        >>>
        writeDocument [withIndent yes] output
    return ()
