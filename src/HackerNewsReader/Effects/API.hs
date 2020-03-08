{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module HackerNewsReader.Effects.API where

import Control.Concurrent.Async
import Data.Proxy
import Data.Text (Text, pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Polysemy
import Polysemy.Reader
import Servant.API
import Servant.Client

import HackerNewsReader.Item
import HackerNewsReader.User

-- | Type-alias for the id of an item
type ItemId = Integer

-- | Define the HN API using types with Servant.
type HackerNewsAPI = "item" :> Capture "id" Text :> Get '[JSON] Item
                :<|> "user" :> Capture "id" Text :> Get '[JSON] User
                :<|> "topstories.json" :> Get '[JSON] [ItemId]
                :<|> "newstories.json" :> Get '[JSON] [ItemId]
                :<|> "beststories.json" :> Get '[JSON] [ItemId]


hackerNewsAPI :: Proxy HackerNewsAPI
hackerNewsAPI = Proxy

-- | Create client functions to use the API.
item :<|> user :<|> top :<|> new :<|> best = client hackerNewsAPI

-- | Appends ".json" to an ItemId to query the API properly.
getItem :: ItemId -> ClientM Item
getItem id = item $ pack $ show id <> ".json"

-- | Appends ".json" to a user's id to query the API properly.
getUser :: Text -> ClientM User
getUser id = user (id <> ".json")

-- | Define the HNAPI effect.
data HNAPI m a where
    FetchUser :: Text -> HNAPI m (Either ClientError User)
    FetchItem :: Integer -> HNAPI m (Either ClientError Item)
    FetchItemAsync :: Integer -> HNAPI m (Async (Either ClientError Item))
    WaitApi :: (Async (Either ClientError a)) -> HNAPI m (Either ClientError a)
    FetchTop  :: HNAPI m (Either ClientError [Integer])
    FetchNew  :: HNAPI m (Either ClientError [Integer])
    FetchBest :: HNAPI m (Either ClientError [Integer])

makeSem ''HNAPI

-- | Interpret the HNAPI effect in IO. This requires a Reader effect containing the
-- ClientEnv so requests can be made.
runHNApiIO :: ( Member (Embed IO) r
              , Member (Reader ClientEnv) r
              ) => Sem (HNAPI ': r) a -> Sem r a
runHNApiIO = interpret $ \v -> do
    env <- ask @ClientEnv
    out <- case v of
        FetchUser name -> embed $ runClientM (getUser name) env
        FetchItem id -> embed $ runClientM (getItem id) env
        FetchItemAsync id -> embed $ async $ runClientM (getItem id) env
        WaitApi a -> embed $ wait a
        FetchTop -> embed $ runClientM top env
        FetchNew -> embed $ runClientM new env
        FetchBest -> embed $ runClientM best env
    pure out
{-# INLINE runHNApiIO #-}
