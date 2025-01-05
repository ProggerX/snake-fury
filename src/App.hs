{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, evalStateT, gets)
import EventQueue (EventQueue, HasEventQueue (..), readEvent, setSpeed)
import GameState (Event (..), GameState, HasGameState (..), move)
import RenderState (
  BoardInfo,
  HasBoardInfo (..),
  HasRenderState (..),
  RenderMessage,
  RenderState (score),
  gameOver,
  render,
  updateMessages,
 )

data AppState = AppState GameState RenderState
data Env = Env BoardInfo EventQueue
newtype App m a = App {runApp :: ReaderT Env (StateT AppState m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState AppState
    , MonadReader Env
    , MonadIO
    )

-- We need to make AppState and instance of HasGameState so we can use it with functions from `GameState.hs`
instance HasGameState AppState where
  getGameState (AppState gs _) = gs
  setGameState (AppState _ rs) gs' = AppState gs' rs

-- We need to make AppState and instance of HasRenderState so we can use it with functions from `RenderState.hs`
instance HasRenderState AppState where
  getRenderState (AppState _ rs) = rs
  setRenderState (AppState gs _) = AppState gs

instance HasBoardInfo Env where
  getBoardInfo (Env brd _) = brd

instance HasEventQueue Env where
  getEventQueue (Env _ queue) = queue

class (Monad m) => MonadQueue m where
  pullEvent ::
    -- | Pull an Event from the queue
    m Event

class (Monad m) => MonadSnake m where
  updateGameState :: Event -> m [RenderMessage]
  updateRenderState :: [RenderMessage] -> m ()

class (Monad m) => MonadRender m where
  render :: m ()

instance (MonadIO m) => MonadQueue (App m) where
  pullEvent = App $ asks getEventQueue >>= liftIO . readEvent

instance (MonadIO m) => MonadSnake (App m) where
  updateGameState = move
  updateRenderState = updateMessages

instance (MonadIO m) => MonadRender (App m) where
  render = RenderState.render

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: (MonadReader env m, HasEventQueue env, MonadState state m, HasRenderState state, MonadIO m) => m Int
setSpeedOnScore = do
  queue <- asks getEventQueue
  s <- gets (score . getRenderState)
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
  isGameOver <- gets (gameOver . getRenderState)
  unless isGameOver gameloop

-- Run the application as usual
run :: Env -> AppState -> IO ()
run env app = runApp gameloop `runReaderT` env `evalStateT` app
