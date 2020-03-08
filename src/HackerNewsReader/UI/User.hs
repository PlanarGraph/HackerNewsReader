{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module HackerNewsReader.UI.User where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Lens (makeLenses, (^.), (.~), (&))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vt

import HackerNewsReader.Actions
import HackerNewsReader.User
import HackerNewsReader.Item

-- | Datatype containing the state for the user UI.
data UserUI = UserUI
    { _user :: User
    , _submitted :: GenericList () V.Vector Item
    , _action :: Action
    }

makeLenses ''UserUI

-- | Defines the app type to use with the defaultMain function
userApp = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appStartEvent = return
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrMap Vt.defAttr []
    }

type MyEvent = ()

-- | Event handler for the user UI.
-- Esc exits the program.
-- q returns to the previous UI.
-- Enter views the selected item if it is a story item (a post), otherwise it does nothing.
-- All other input is sent to the list handler, which moves the cursor
-- with the arrow keys.
handleEvent :: UserUI -> BrickEvent () MyEvent -> EventM () (Next UserUI)
handleEvent ui (VtyEvent (Vt.EvKey Vt.KEsc [])) = halt $ ui & action .~ Exit
handleEvent ui (VtyEvent (Vt.EvKey (Vt.KChar 'q') [])) = halt $ ui & action .~ Back
handleEvent ui (VtyEvent (Vt.EvKey Vt.KEnter [])) =
    let (_, sel) = fromJust $ listSelectedElement $ ui ^. submitted in
        case itemType sel of
            Just "story" -> halt $ ui & action .~ LoadPost (itemId sel)
            _ -> continue ui
handleEvent ui (VtyEvent v) = do
    l <- handleListEvent v $ ui ^. submitted
    continue $ ui & submitted .~ l 

drawUI :: UserUI -> [Widget ()]
drawUI ui = [drawUser ui]

drawUser :: UserUI -> Widget ()
drawUser ui = padRight Max $
    borderWithLabel (txt "User") $
    vBox [ txt $ userId u
         , txt $ "Karma: " <> karma u
         , txtWrap $ maybe "" id $ userAbout u
         , hBorder
         , renderList drawSubmitted True $ ui ^. submitted
         ]
  where
    u = ui ^. user
    karma = T.pack . show . userKarma

drawSubmitted :: Bool -> Item -> Widget ()
drawSubmitted b i = (if b then cursor else emptyWidget) 
    <+> (border $ padRight Max $ renderItem i)
  where
    cursor = vBox [ txt "\n", txt "\n", txt ">" ]

renderItem :: Item -> Widget ()
renderItem i = 
    case itemType i of
        Just "story" -> 
            vBox [ txt $ "story " <> iid <> ", score: " <> score
                 , txtWrap $ "title: " <> title
                 , if T.null text then emptyWidget else txtWrap $ "text: " <> text
                 ]
        Just "job" ->
            vBox [ txt $ "job " <> iid <> ", score: " <> score
                 , txtWrap $ "title: " <> title
                 , txtWrap $ "text: " <> text
                 ]
        Just "comment" ->
            vBox [ txt $ "comment " <> iid
                 , txtWrap $ "text: " <> text
                 ]
        Just "poll" ->
            vBox [ txt $ "poll " <> iid <> ", score: " <> score
                 , txtWrap $ "title: " <> title
                 ]
        _ -> emptyWidget
  where
    iid = T.pack $ show $ itemId i
    title = maybe "" id $ itemTitle i
    text = maybe "" id $ itemText i
    score = T.pack $ show $ maybe 0 id $ itemScore i
    
