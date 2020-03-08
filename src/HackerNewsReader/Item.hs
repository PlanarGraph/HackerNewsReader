{-# LANGUAGE OverloadedStrings #-}
module HackerNewsReader.Item where

import Data.Aeson
import Control.Monad (mzero)
import Data.Text (Text, unpack)

import HTMLEntities.Decoder
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy as TL

-- | Datatype corresponding to the items API of HN.
-- This contains only the fields I was interested in using.
data Item = Item
    { itemId :: Integer
    , itemBy :: Text
    , itemKids :: Maybe [Integer]
    , itemUrl :: Maybe Text
    , itemScore :: Maybe Integer
    , itemTitle :: Maybe Text
    , itemText :: Maybe Text
    , itemType :: Maybe Text
    } deriving (Eq)

instance FromJSON Item where
    parseJSON (Object o) =
        Item <$> o .: "id"
             <*> o .: "by"
             <*> o .:? "kids"
             <*> o .:? "url"
             <*> o .:? "score"
             <*> o .:? "title"
             <*> o .:? "text"
             <*> o .:? "type"

    parseJSON _ = mzero

instance Show Item where
    show i = "Id: " <> (show $ itemId i) <> " " <> "by: " <> (unpack $ itemBy i)

-- | Partially fixes the text of an item, converting the html it contains to normal text
-- and replacing the <p> tags with newlines.
fixText :: Item -> Item
fixText i = i { itemText = fmap (replaceP . removeEncoding) $ itemText i }
  where
    removeEncoding = LB.toLazyText . htmlEncodedText
    replaceP = TL.toStrict . TL.replace "<p>" "\n\n"
