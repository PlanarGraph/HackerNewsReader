{-# LANGUAGE OverloadedStrings #-}
module HackerNewsReader.User where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)

import HTMLEntities.Decoder
-- import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.Text.Lazy as TL

-- | Datatype represting the users API, containing only
-- the fields I was intersted in.
data User = User
    { userId :: Text
    , userKarma :: Integer
    , userAbout :: Maybe Text
    , userSubmitted :: Maybe [Integer]
    } deriving (Eq, Show)

instance FromJSON User where
    parseJSON (Object o) = 
        User <$> o .: "id"
             <*> o .: "karma"
             <*> o .:? "about"
             <*> o .:? "submitted"

    parseJSON _ = mzero

fixAbout :: User -> User
fixAbout u = u { userAbout = fmap (replaceP . removeEncoding) $ userAbout u }
  where
    removeEncoding = LB.toLazyText . htmlEncodedText
    replaceP = TL.toStrict . TL.replace "<p>" "\n\n"
