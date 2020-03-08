{-# LANGUAGE TemplateHaskell #-}
module HackerNewsReader.Effects.Stack where

import Polysemy
import Polysemy.State

-- | Defines a simple stack effect.
data Stack v m a where
    Push :: v -> Stack v m ()   -- ^ Pushes an item onto the stack.
    Pop  :: Stack v m (Maybe v) -- ^ Pops an item from the stack if there is one.
    Peek :: Stack v m (Maybe v) -- ^ Peeks the top item on the stack if there is one.

makeSem ''Stack

-- | Reinterprets the stack effect as a state effect with a list of values as
-- the stack.
runStackAsState :: Sem (Stack v ': r) a -> Sem (State [v] ': r) a
runStackAsState = reinterpret $ \case
    Push v -> modify $ (:) v
    Pop -> do
        l <- get
        case l of
            [] -> pure Nothing
            (x : xs) -> do
                put xs
                pure $ Just x
    Peek -> do
        l <- get
        case l of
            [] -> pure Nothing
            (x : xs) -> pure $ Just x
{-# INLINE runStackAsState #-}
