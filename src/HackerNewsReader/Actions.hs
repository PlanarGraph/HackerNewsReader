module HackerNewsReader.Actions where

import Data.Text (Text)

-- | Datatype representing an output action for the UIs.
data Action
    = LoadUser Text
    | LoadPost Integer
    | Back
    | Exit
