{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}

{- |
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.

import Control.Lens (use, (.=))
import Control.Monad (when)
import Control.Monad.RWS.Strict (RWST)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, state)
import Data.Foldable (toList)
import Data.Sequence (Seq ((:|>)), (<|))
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import RenderState (
  BoardInfo (..),
  CellType (..),
  DeltaBoard,
  Point,
  RenderMessage (..),
 )
import System.Random (Random (randomR), StdGen)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Movement

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

{- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
  In first sight we'd define the snake as a sequence, but If you think carefully, an empty
  sequence can't represent a valid Snake, therefore we must use a non empty one.
  You should investigate about Seq type in haskell and we it is a good option for our porpouse.
-}

-- NOTE: Body is never empty
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

{- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and
  a random seed to calculate the next random apple.
-}
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Eq, Generic, Show)

type GameStep = RWST BoardInfo () GameState

-- | This function should calculate the opposite movement.
oppositeMovement :: Movement -> Movement
oppositeMovement = \case
  North -> South
  South -> North
  West -> East
  East -> West

{- | Purely creates a random point within the board limits
  You should take a look to System.Random documentation.
  Also, in the import list you have all relevant functions.
-}
makeRandomPoint :: Monad m => GameStep m Point
makeRandomPoint = do
  BoardInfo{height, width} <- ask
  state $ #randomGen $ randomR ((1, 1), (height, width))

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}

-- | Check if a point is in the snake
snakePoints :: SnakeSeq -> [Point]
snakePoints SnakeSeq{snakeHead, snakeBody} = snakeHead : toList snakeBody

inSnake :: Point -> SnakeSeq -> Bool
inSnake pt snake = pt `elem` snakePoints snake

{-
This is a test for inSnake. It should return
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

{- | Calculates de new head of the snake. Considering it is moving in the current direction
  Take into acount the edges of the board
-}
nextHead :: BoardInfo -> GameState -> Point
nextHead BoardInfo{height, width} GameState{snakeSeq = SnakeSeq{snakeHead = (y, x)}, movement} =
  case movement of
    South -> (if y == height then 1 else y + 1, x)
    North -> (if y == 1 then height else y - 1, x)
    East -> (y, if x == width then 1 else x + 1)
    West -> (y, if x == 1 then width else x - 1)

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: Monad m => GameStep m Point
newApple = do
  pt <- makeRandomPoint
  GameState{snakeSeq, applePosition} <- get
  if inSnake pt snakeSeq || pt == applePosition
    then newApple
    else do
      #applePosition .= pt
      pure pt

{- We can't test this function because it depends on makeRandomPoint -}

{- | Moves the snake based on the current direction. It sends the adequate RenderMessage
Notice that a delta board must include all modified cells in the movement.
For example, if we move between this two steps
       - - - -          - - - -
       - 0 $ -    =>    - - 0 $
       - - - -    =>    - - - -
       - - - X          - - - X
We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]

Another example, if we move between this two steps
       - - - -          - - - -
       - - - -    =>    - X - -
       - - - -    =>    - - - -
       - 0 $ X          - 0 0 $
We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-}
step :: Monad m => GameStep m [RenderMessage]
step = do
  st@GameState{snakeSeq = snake@SnakeSeq{snakeBody}, applePosition} <- get
  brd@BoardInfo{height, width} <- ask
  let head' = nextHead brd st
  if
    | length snakeBody == height * width - 2 || inSnake head' snake ->
        pure [GameOver]
    | head' == applePosition -> do
        msg <- extendSnake head'
        pure [IncrementScore, RenderBoard msg]
    | otherwise -> do
        msg <- displaceSnake head'
        pure [RenderBoard msg]

move :: Monad m => Event -> GameStep m [RenderMessage]
move event = do
  currentMovement <- use #movement
  case event of
    Tick -> pure ()
    UserEvent userMovement ->
      when (userMovement /= oppositeMovement currentMovement) $
        #movement .= userMovement
  step

seqInit :: Seq a -> Seq a
seqInit = \case
  s :|> _ -> s
  Seq.Empty -> Seq.Empty

extendSnake :: Monad m => Point -> GameStep m DeltaBoard
extendSnake head' = do
  SnakeSeq{snakeHead, snakeBody} <- use #snakeSeq
  #snakeSeq .= SnakeSeq head' (snakeHead <| snakeBody)
  applePosition' <- newApple
  pure
    [ (applePosition', Apple)
    , (snakeHead, Snake)
    , (head', SnakeHead)
    ]

displaceSnake :: Monad m => Point -> GameStep m DeltaBoard
displaceSnake head' = do
  SnakeSeq{snakeHead, snakeBody} <- use #snakeSeq
  #snakeSeq .= SnakeSeq head' (snakeHead <| seqInit snakeBody)
  pure
    [ (snakeHead, Snake)
    , (head', SnakeHead)
    , (snakeBody `Seq.index` (length snakeBody - 1), Empty)
    ]
