module Initialization where

import Control.Concurrent (newMVar)
import Control.Concurrent.BoundedChan (newBoundedChan)
import Data.Sequence qualified as Seq
import EventQueue (EventQueue (EventQueue))
import GameState qualified as Snake
import RenderState qualified
import System.Random (getStdGen, randomRIO)

-- | Produces a random point. Use for game initialization, random point generation is done purely within Snake module.
getRandomPoint :: Int -> Int -> IO RenderState.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

-- | Sample two random points (snake head and apple) ensuring they are different
inititalizePoints :: Int -> Int -> IO (RenderState.Point, RenderState.Point)
inititalizePoints h w = do
  (snakeInit, appleInit) <- (,) <$> getRandomPoint h (w - 1) <*> getRandomPoint h w
  if snakeInit == appleInit
    then inititalizePoints h w
    else return (snakeInit, appleInit)

-- | given the initial parameters height, width and initial time, It creates the initial state, the initial render state and the event queue
gameInitialization :: Int -> Int -> Int -> IO (RenderState.BoardInfo, Snake.GameState, RenderState.RenderState, EventQueue)
gameInitialization hight width initialspeed = do
  (snakeInit, appleInit) <- inititalizePoints hight width
  sg <- getStdGen
  newUserEventQueue <- newBoundedChan 3
  newSpeed <- newMVar initialspeed
  let binf = RenderState.BoardInfo hight width
      gameState = Snake.GameState (Snake.SnakeSeq snakeInit $ Seq.fromList [(fst snakeInit, snd snakeInit - 1)]) appleInit Snake.East sg
      renderState = RenderState.buildInitialBoard binf snakeInit appleInit
      eventQueue = EventQueue newUserEventQueue newSpeed initialspeed
  return (binf, gameState, renderState, eventQueue)
