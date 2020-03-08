{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module HackerNewsReader.UI.FrontPage where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Lens
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vt

import HackerNewsReader.Actions
import HackerNewsReader.Item

-- | Datatype containing the state of the front page.
data FrontPageUI = FrontPageUI
    { _items :: List () Item
    , _selected :: Integer
    , _action :: Action
    }

makeLenses ''FrontPageUI

-- | Defines the app type to use with the defaultMain function
frontPageApp = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appStartEvent = return
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrMap Vt.defAttr []
    }

type MyEvent = ()

-- | Event handler for the front page UI.
-- Esc and q exit the program.
-- u views the user page for the user of the selected post.
-- Enter views the selected post.
-- All other input is sent to the list handler, which moves the cursor
-- with the arrow keys.
handleEvent :: FrontPageUI -> BrickEvent () MyEvent -> EventM () (Next FrontPageUI)
handleEvent ui (VtyEvent (Vt.EvKey Vt.KEsc [])) = halt $ ui & action .~ Exit
handleEvent ui (VtyEvent (Vt.EvKey (Vt.KChar 'q') [])) = halt $ ui & action .~ Exit
handleEvent ui (VtyEvent (Vt.EvKey (Vt.KChar 'u') [])) =
    let (_, sel) = fromJust $ listSelectedElement $ ui ^. items in
        halt $ ui & action .~  LoadUser (itemBy sel)
handleEvent ui (VtyEvent (Vt.EvKey Vt.KEnter [])) =
    let (_, sel) = fromJust $ listSelectedElement $ ui ^. items in
        halt $ ui & action .~ LoadPost (itemId sel)
handleEvent ui (VtyEvent v) = do
    items' <- handleListEvent v $ ui ^. items
    continue $ ui & items .~ items'

-- Widgets
drawUI :: FrontPageUI -> [Widget ()]
drawUI ui = 
    [ borderWithLabel (str "Front Page") $ renderList renderListItem True $ ui ^. items ]

renderItem :: Item -> Widget ()
renderItem i = padRight Max $ 
    vBox [ title
         , hyperlink url $ txt url
         , txt $ score <> " points by " <> author 
         ]
  where
    url = maybe "" id $ itemUrl i
    title = txtWrap $ fromJust $ itemTitle i
    author = itemBy i
    score = T.pack $ show $ maybe 0 id $ itemScore i

renderListItem :: Bool -> Item -> Widget ()
renderListItem b i = (if b then cursor else emptyWidget) <+>
    (withBorderStyle unicode $ border $ renderItem i )
  where
    cursor = vBox [ txt "\n", txt "\n", txt ">" ]

