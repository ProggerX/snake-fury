{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets)
import EventQueue (EventQueue, readEvent, setSpeed)
import GameState (GameState, HasGameState, move)
import GameState qualified
import RenderState (
  BoardInfo,
  HasRenderState,
  RenderState,
  gameOver,
  getRenderState,
  render,
  score,
 )
import RenderState qualified

-- This is the new state, which glue together Game and Render states.
data AppState = AppState GameState RenderState

-- Our application is a readerT with and AppState and IO capabilities.
newtype App m a = App {runApp :: ReaderT BoardInfo (StateT AppState m) a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader BoardInfo
    , MonadState AppState
    )

-- We need to make AppState and instance of HasGameState so we can use it with functions from `GameState.hs`
instance HasGameState AppState where
  getGameState (AppState gs _) = gs
  setGameState (AppState _ rs) gs' = AppState gs' rs

-- We need to make AppState and instance of HasRenderState so we can use it with functions from `RenderState.hs`
instance HasRenderState AppState where
  getRenderState (AppState _ rs) = rs
  setRenderState (AppState gs _) = AppState gs

-- This function should read an event from the queue, move the snake and render.
-- Notice the constraints.
gameStep :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameStep queue = liftIO (readEvent queue) >>= move >>= render

-- The gameloop is easy as hell. Read the score and wait the requiered time. Then run the gameStep.
-- This function is implemented for easiness.
gameloop :: (MonadReader BoardInfo m, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameloop queue = do
  s <- gets (score . getRenderState)
  new_speed <- liftIO $ setSpeed s queue
  liftIO $ threadDelay new_speed
  gameStep queue
  game_over <- gets (gameOver . getRenderState)
  unless game_over (gameloop queue)

-- This function runs the gameloop.
run :: BoardInfo -> AppState -> EventQueue -> IO ()
run binf app queue = gameloop queue `evalStateT` app `runReaderT` binf
