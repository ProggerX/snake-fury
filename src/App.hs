{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module App where

import Control.Concurrent (threadDelay)
import Control.Lens (magnify, use, view, zoom)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (MonadReader, MonadState, RWST, evalRWST)
import EventQueue (EventQueue, HasEventQueue, readEvent, setSpeed)
import GHC.Generics (Generic)
import GameState (Event (..), GameState, move)
import RenderState (
  BoardInfo,
  HasRenderState,
  RenderMessage,
  RenderState,
  render,
  updateMessages,
 )

data AppState = AppState {gameState :: GameState, renderState :: RenderState}
  deriving (Generic)

data Env = Env {boardInfo :: BoardInfo, eventQueue :: EventQueue}
  deriving (Generic)

newtype App a = App (RWST Env () AppState IO a)
  deriving
    (Applicative, Functor, Monad, MonadIO, MonadState AppState, MonadReader Env)

runApp :: Env -> AppState -> App a -> IO a
runApp env initialState (App app) = fst <$> evalRWST app env initialState

class (Monad m) => MonadQueue m where
  -- | Pull an Event from the queue
  pullEvent :: m Event

class (Monad m) => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class (Monad m) => MonadRender m where
  render :: m ()

instance MonadQueue App where
  pullEvent = App $ view #eventQueue >>= liftIO . readEvent

instance MonadSnake App where
  updateGameState event =
    App $ zoom #gameState $ magnify #boardInfo $ move event

  --  where
  --   zoom l = state . l . runState

  updateRenderState = updateMessages

instance MonadRender App where
  render = RenderState.render

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = do
  queue <- view #eventQueue
  s <- use $ #renderState . #score
  liftIO $ setSpeed s queue

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: (MonadQueue m, MonadSnake m, MonadRender m) => m ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >>= pure App.render

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: (MonadQueue m, MonadSnake m, MonadRender m, MonadState state m, HasRenderState state, MonadReader env m, HasEventQueue env, MonadIO m) => m ()
gameloop = do
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep
  isGameOver <- use $ #renderState . #gameOver
  unless isGameOver gameloop

run :: Env -> AppState -> IO ()
run env initialState = runApp env initialState gameloop
