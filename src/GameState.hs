{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local)
import Control.Monad.State.Strict (MonadState, StateT, get, put)
import Data.Foldable (toList)
import Data.Sequence (Seq ((:|>)), (<|))
import Data.Sequence qualified as Seq
import RenderState (
  BoardInfo (BoardInfo),
  CellType (Apple, Empty, Snake, SnakeHead),
  DeltaBoard,
  Point,
  RenderMessage (GameOver, IncrementScore, RenderBoard),
 )
import RenderState qualified
import System.Random (StdGen, randomR)

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
  deriving (Show, Eq)

newtype GameStep m a = GameStep {runGameStep :: ReaderT BoardInfo (StateT GameState m) a}

class HasGameState state where
  getGameState :: state -> GameState
  setGameState :: state -> GameState -> state

instance (Functor m) => Functor (GameStep m) where
  fmap f (GameStep ma) = GameStep (fmap f ma)

instance (Monad m) => Applicative (GameStep m) where
  pure a = GameStep (pure a)
  (GameStep mf) <*> (GameStep ma) = GameStep (mf <*> ma)

instance (Monad m) => Monad (GameStep m) where
  (GameStep ma) >>= f = GameStep $ ma >>= (runGameStep . f)

instance (Monad m) => (MonadState GameState) (GameStep m) where
  get :: GameStep m GameState
  get = GameStep get
  put :: GameState -> GameStep m ()
  put a = GameStep $ put a

instance (Monad m) => (MonadReader BoardInfo) (GameStep m) where
  ask :: GameStep m BoardInfo
  ask = GameStep ask
  local :: (BoardInfo -> BoardInfo) -> GameStep m a -> GameStep m a
  local f (GameStep ma) = GameStep $ local f ma

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
makeRandomPoint :: (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m Point
makeRandomPoint = do
  BoardInfo{height, width} <- ask
  zoomRandomGen $ randomR ((1, 1), (height, width))

zoomRandomGen ::
  (MonadState s m, HasGameState s) => (StdGen -> (a, StdGen)) -> m a
zoomRandomGen f = do
  as <- get
  let st = getGameState as
  let (a, randomGen) = f st.randomGen
  put $ setGameState as st{randomGen}
  pure a

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
newApple :: (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m Point
newApple = do
  pt <- makeRandomPoint
  as <- get
  let st@GameState{snakeSeq, applePosition} = getGameState as
  if inSnake pt snakeSeq || pt == applePosition
    then newApple
    else do
      put $ setGameState as st{applePosition = pt}
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
step ::
  (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m [RenderMessage]
step = do
  st@GameState{snakeSeq = snake@SnakeSeq{snakeBody}, applePosition} <- getGameState <$> get
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

move ::
  (MonadReader BoardInfo m, MonadState state m, HasGameState state) =>
  Event ->
  m [RenderMessage]
move event = do
  as <- get
  let gstate = getGameState as
  case event of
    Tick -> pure ()
    UserEvent movement -> do
      when (gstate.movement /= oppositeMovement movement) $
        put $
          setGameState as gstate{movement}
  step

seqInit :: Seq a -> Seq a
seqInit = \case
  s :|> _ -> s
  Seq.Empty -> Seq.Empty

extendSnake :: Point -> (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m DeltaBoard
extendSnake head' = do
  as <- get
  let st@GameState{snakeSeq = SnakeSeq{snakeHead, snakeBody}} = getGameState as
  put $ setGameState as st{snakeSeq = SnakeSeq head' $ snakeHead <| snakeBody}
  applePosition' <- newApple
  pure
    [ (applePosition', Apple)
    , (snakeHead, Snake)
    , (head', SnakeHead)
    ]

displaceSnake :: Point -> (MonadState s m, HasGameState s, MonadReader BoardInfo m) => m DeltaBoard
displaceSnake head' = do
  as <- get
  let st@GameState{snakeSeq = SnakeSeq{snakeHead, snakeBody}} = getGameState as
  put $ setGameState as st{snakeSeq = SnakeSeq head' $ snakeHead <| seqInit snakeBody}
  pure
    [ (snakeHead, Snake)
    , (head', SnakeHead)
    , (snakeBody `Seq.index` (length snakeBody - 1), Empty)
    ]
