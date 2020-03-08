module HackerNewsReader (runHN) where

import Control.Lens
import Data.Either
import Data.Function ((&))
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Tree 
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Polysemy
import Polysemy.KVStore
import Polysemy.Reader
import Polysemy.State
import Servant.Client

import HackerNewsReader.Effects.API
import HackerNewsReader.Effects.Stack
import HackerNewsReader.Effects.UI
import HackerNewsReader.Actions
import HackerNewsReader.Item
import HackerNewsReader.User
import HackerNewsReader.UI.FrontPage
import HackerNewsReader.UI.Post
import HackerNewsReader.UI.User

-- | Outputstate is a wrapper for the three different UIs.
data OutputState
    = FPage FrontPageUI
    | PostPage PostUI
    | UserPage UserUI

-- | Traverses the item's children, asynchronously loading all of the comments
-- for a post.
loadPostTree :: Members '[Reader ClientEnv, HNAPI, KVStore ItemId Item] r
             => Item -> Sem r (Tree Item)
loadPostTree it = do
    kids <- (lookupCachedItems $ maybe [] id $ itemKids it)
        >>= traverse loadPostTree
    pure $ Node it kids

-- | Taking a list of 'ItemId's, this looks up the items in a `KVStore` cache.
-- If they are there, load them from the cache. The reset are asynchronously loaded
-- and then cached for later use.
lookupCachedItems :: Members '[HNAPI, Reader ClientEnv, KVStore ItemId Item] r
                  => [ItemId] -> Sem r [Item] 
lookupCachedItems it = do
    cached <- catMaybes <$> traverse lookupKV it
    let cachedIds = map itemId cached
    uncached <- traverse fetchItemAsync (it \\ cachedIds)
        >>= traverse waitApi
        >>= pure . fmap fixText . rights
    traverse (uncurry writeKV) [ (itemId u, u) | u <- uncached ]
    pure $ uncached <> cached

-- | Gets the first 30 items from the top posts of HN, loads/caches them, and then
-- runs the front page UI.
loadFrontPage :: Members '[HNAPI, Reader ClientEnv, HNUI, KVStore ItemId Item] r
              => Sem r FrontPageUI
loadFrontPage = do
    items <- getTop 30 <$> fetchTop >>= lookupCachedItems
    viewFrontPage items
  where
    getTop n = take n . fromRight []

-- | Loads a post and its comments, and then runs the post UI.
loadPost :: Members '[HNAPI, Reader ClientEnv, HNUI, KVStore ItemId Item] r
         => Item -> Sem r PostUI
loadPost it = do
    pt <- loadPostTree it
    viewPost pt

-- | Loads a user and the 30 most recent items submitted by them. These items are
-- cached, and then the user UI is displayed.
-- 
-- This function throws an error if it is unable to load the user.
loadUser :: Members '[HNAPI, Reader ClientEnv, HNUI, KVStore ItemId Item] r
         => Text -> Sem r UserUI
loadUser u = do
    us <- fetchUser u 
    case us of
        Left _ -> error "Cannot retrieve user"
        Right user -> do
            subs <- lookupCachedItems . getLast 30 $ userSubmitted user
            viewUser (fixAbout user) subs
  where
    getLast n = take n . maybe [] id


-- | The main HN program. Runs the front page once, then enters the program loop 'lp'.
-- 'lp' handles the output actions of each UI, making use of a stack effect to go
-- forward and back through the different application states.
hn :: Members '[ HNAPI
               , Reader ClientEnv
               , HNUI
               , KVStore ItemId Item
               , Stack OutputState
               ] r => Sem r ()
hn = loadFrontPage >>= lp . FPage
  where
    lp :: Members '[ HNAPI
                   , Reader ClientEnv
                   , HNUI
                   , KVStore ItemId Item
                   , Stack OutputState
                   ] r => OutputState -> Sem r ()
    lp page =
        case act page of
            Exit -> pure ()
            LoadPost n -> do
                push page
                it <- fromJust <$> (lookupKV @ItemId @Item) n
                ui <- loadPost it
                lp $ PostPage ui
            LoadUser u -> do
                push page
                ui <- loadUser u
                lp $ UserPage ui
            Back -> do
                page' <- pop
                case page' of
                    Just (UserPage ui) -> do 
                        ui' <- viewPrevUser ui
                        lp $ UserPage ui'
                    Just (FPage ui) -> do
                        ui' <- viewPrevFrontPage ui
                        lp $ FPage ui'
                    Just (PostPage ui) -> do
                        ui' <- viewPrevPost ui
                        lp $ PostPage ui'

    -- | Converts an OutputState to its output action.
    act :: OutputState -> Action
    act (FPage ui)    = ui ^. HackerNewsReader.UI.FrontPage.action
    act (PostPage ui) = ui ^. HackerNewsReader.UI.Post.action
    act (UserPage ui) = ui ^. HackerNewsReader.UI.User.action

-- | Runs the 'hn' program in IO, interpreting all its effects along the way.
runHN :: IO ()
runHN = do
    manager <- newManager tlsManagerSettings
    intp manager
  where
    intp manager = hn
        & runHNApiIO
        & runHNUiIO
        & runReader (mkClientEnv manager (BaseUrl Https "hacker-news.firebaseio.com" 443 "/v0"))
        & runKVStoreAsState
        & evalState (mempty :: Map ItemId Item)
        & runStackAsState
        & evalState (mempty :: [OutputState])
        & runM
