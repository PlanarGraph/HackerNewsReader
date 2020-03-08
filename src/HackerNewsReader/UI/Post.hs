{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module HackerNewsReader.UI.Post where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Lens (makeLenses, (^.), (.~), (&))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Tree
import qualified Data.Vector as V
import qualified Graphics.Vty as Vt

import HackerNewsReader.Actions
import HackerNewsReader.Item

-- | Datatype containing the state for the posts UI.
data PostUI = PostUI
    { _post :: Item
    , _comments :: GenericList () V.Vector (Int, Item)
    , _action :: Action
    }

makeLenses ''PostUI

-- | Defines the app type to use with the defaultMain function
postApp = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appStartEvent = return
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrMap Vt.defAttr []
    }

type MyEvent = ()

-- | Event handler for the post UI.
-- Esc exits the program.
-- q returns to the previous UI.
-- u views the user page of the selected item.
-- All other input is sent to the list handler, which moves the cursor
-- with the arrow keys.
handleEvent :: PostUI -> BrickEvent () MyEvent -> EventM () (Next PostUI)
handleEvent ui (VtyEvent (Vt.EvKey Vt.KEsc [])) = halt $ ui & action .~ Exit
handleEvent ui (VtyEvent (Vt.EvKey (Vt.KChar 'q') [])) = halt $ ui & action .~ Back
handleEvent ui (VtyEvent (Vt.EvKey (Vt.KChar 'u') [])) =
    let (_, (_, sel)) = fromJust $ listSelectedElement $ ui ^. comments in
        halt $ ui & action .~  LoadUser (itemBy sel)
handleEvent ui (VtyEvent v) = do
    l <- handleListEvent v $ ui ^. comments
    continue $ ui & comments .~ l 

-- | Converts a tree of items (a post and comments) to the UI state type.
treeToPostUI :: Tree Item -> PostUI
treeToPostUI t = PostUI a gen Exit
    where
      (a : as) = flatten t
      (_ : bs) = levels t
      mp = M.fromList [ (itemId i, l) | (l, it) <- zip [1..] bs, i <- it ]
      marked = V.fromList [ (M.findWithDefault 0 (itemId i) mp, i) | i <- as ]
      gen = list () marked 3
      
-- Widgets
drawUI :: PostUI -> [Widget ()]
drawUI ui = [drawPost ui]

drawPost :: PostUI -> Widget ()
drawPost ui = padRight Max $
    vBox [ txtWrap title
         , txt $ points <> " points by " <> (itemBy $ ui ^. post)
         , hBorder
         , renderList drawComments True $ ui ^. comments
         ]
  where
    title = fromJust $ itemTitle $ ui ^. post
    points = T.pack $ show $ fromJust $ itemScore $ ui ^. post

drawComments :: Bool -> (Int, Item) -> Widget ()
drawComments b (l, i) = hBox $ [if b then cursor else emptyWidget] <> hline <>
    [vBox 
        [ txt $ (itemBy i) <> " says: "
        , txtWrap $ fromJust $ itemText i
        , txt "\n"
        ]]
  where
    hline = replicate l (txt "    ") --replicate (fromIntegral l) (vLimit 2 $ vBorder)
    cursor = vBox [ txt "\n", txt ">" ]
