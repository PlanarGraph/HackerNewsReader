{-# LANGUAGE TemplateHaskell #-}
module HackerNewsReader.Effects.UI where

import Brick.Main (defaultMain)
import Brick.Widgets.List (list)
import Data.Maybe (isJust)
import Data.Tree (Tree)
import qualified Data.Vector as V
import Polysemy

import HackerNewsReader.Actions
import HackerNewsReader.Item
import HackerNewsReader.User
import HackerNewsReader.UI.FrontPage
import HackerNewsReader.UI.Post
import HackerNewsReader.UI.User

-- | Define the HNUI effect for displaying the program's different UIs.
data HNUI m a where
    ViewFrontPage :: [Item] -> HNUI m FrontPageUI
    ViewPost :: Tree Item -> HNUI m PostUI
    ViewUser :: User -> [Item] -> HNUI m UserUI
    ViewPrevPost :: PostUI -> HNUI m PostUI
    ViewPrevUser :: UserUI -> HNUI m UserUI
    ViewPrevFrontPage :: FrontPageUI -> HNUI m FrontPageUI

makeSem ''HNUI

-- | Interprets the UIs in IO.
runHNUiIO :: ( Member (Embed IO) r
             ) => Sem (HNUI ': r) a -> Sem r a
runHNUiIO = interpret $ \case
    ViewFrontPage items -> do
        let fui = FrontPageUI (list () (V.fromList $ filter (isJust . itemTitle) items) 3) 0 Exit in
            embed $ defaultMain frontPageApp fui
    ViewPost pt -> embed $ defaultMain postApp $ treeToPostUI pt
    ViewUser user items ->
        let sub = list () (V.fromList items) 3
            ui = UserUI user sub Exit in
            embed $ defaultMain userApp ui
    ViewPrevPost ui -> embed $ defaultMain postApp ui
    ViewPrevUser ui -> embed $ defaultMain userApp ui
    ViewPrevFrontPage ui -> embed $ defaultMain frontPageApp ui
{-# INLINE runHNUiIO #-}
