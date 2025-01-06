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
import EventQueue (EventQueue, readEvent, setSpeed)
import GHC.Generics (Generic)
import GameState (Event (..), GameState, move)
import RenderState (
  BoardInfo,
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

-- | Pull an Event from the queue
pullEvent :: App Event
pullEvent = App $ view #eventQueue >>= liftIO . readEvent

updateGameState :: Event -> App [RenderMessage]
updateGameState = App . magnify #boardInfo . zoom #gameState . move

updateRenderState :: [RenderMessage] -> App ()
updateRenderState =
  App . magnify #boardInfo . zoom #renderState . updateMessages

render :: App ()
render = App $ magnify #boardInfo $ zoom #renderState RenderState.render

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: App Int
setSpeedOnScore = do
  queue <- view #eventQueue
  s <- use $ #renderState . #score
  liftIO $ setSpeed s queue

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: App ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >>= pure App.render

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: App ()
gameloop = do
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep
  isGameOver <- use $ #renderState . #gameOver
  unless isGameOver gameloop

run :: Env -> AppState -> IO ()
run env initialState = runApp env initialState gameloop
