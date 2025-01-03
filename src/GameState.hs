{-# LANGUAGE LambdaCase #-}

{- |
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.

import Control.Monad.Trans.State.Strict (State, get, modify, put, runState, state)
import Data.Foldable
import Data.Maybe (isJust)
import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq
import RenderState (BoardInfo (..), DeltaBoard, Point)
import RenderState qualified as Board
import System.Random (Random (randomR), RandomGen (split), StdGen, uniformR)

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
  deriving (Show, Eq)

type GameStep a = State GameState a

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement West = East
opositeMovement East = West

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East
-- True
-- True
-- True
-- True

{- | Purely creates a random point within the board limits
  You should take a look to System.Random documentation.
  Also, in the import list you have all relevant functions.
-}
makeRandomPoint :: BoardInfo -> GameStep Point
makeRandomPoint BoardInfo{height, width} = do
  st@GameState{randomGen} <- get
  let (pt, gen') = randomR ((1, 1), (height, width)) randomGen
  put st{randomGen = gen'}
  pure pt

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
newApple :: BoardInfo -> GameStep Point
newApple brd = do
  pt <- makeRandomPoint brd
  modify (\old -> old{applePosition = pt})
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
step :: BoardInfo -> GameStep [Board.RenderMessage]
step brd@BoardInfo{height, width} = do
  st@GameState{snakeSeq = snake@SnakeSeq{snakeBody}, applePosition} <- get
  let head' = nextHead brd st
  if length snakeBody == height * width - 1 || inSnake head' snake
    then pure [Board.GameOver]
    else
      if head' == applePosition
        then do
          msg <- extendSnake head' brd
          pure [Board.IncrementScore, Board.RenderBoard msg]
        else do
          msg <- displaceSnake head' brd
          pure [Board.RenderBoard msg]

move :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
move = runState . step

seqInit :: Seq a -> Seq a
seqInit = \case
  s :|> _ -> s
  Empty -> Empty

extendSnake :: Point -> BoardInfo -> GameStep DeltaBoard
extendSnake head' brd = do
  GameState{snakeSeq = SnakeSeq{snakeHead, snakeBody}} <- get
  modify (\old -> old{snakeSeq = SnakeSeq head' $ snakeHead <| snakeBody})
  applePosition' <- newApple brd

  pure
    [ (applePosition', Board.Apple)
    , (snakeHead, Board.Snake)
    , (head', Board.SnakeHead)
    ]

displaceSnake :: Point -> BoardInfo -> GameStep DeltaBoard
displaceSnake head' _ = do
  GameState{snakeSeq = SnakeSeq{snakeHead, snakeBody}} <- get
  modify (\old -> old{snakeSeq = SnakeSeq head' $ snakeHead <| seqInit snakeBody})
  pure
    [ (snakeHead, Board.Snake)
    , (head', Board.SnakeHead)
    , (snakeBody `Seq.index` (length snakeBody - 1), Board.Empty)
    ]

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
