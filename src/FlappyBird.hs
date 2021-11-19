{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module FlappyBird
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead, score, bird
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

import Constants

-- Types

data Game = Game
  { _bird   :: Bird         -- ^ bird as a coordinate
  , _dir    :: Direction    -- ^ direction
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type Bird = Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = Up
  | Down
  deriving (Eq, Show)

makeLenses ''Game


-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  MaybeT $ guard . not <$> orM [use paused, use dead] -- Make sure the game isn't paused or over
  MaybeT . fmap Just $ locked .= False -- Unlock from last directional turn
  modifying score (+ 1)
  die <|> MaybeT (Just <$> modify move)

-- | Possibly die if next position is either on a barrier cell or above below grid (TODO)
die :: MaybeT (State Game) ()
die = do
  MaybeT . (fmap guard) $ (do 
    { 
      nextPos@(V2 x y) <- (nextPosition <$> get);      --get next position of bird
      birdPositions <- (use bird);
      return (False)        --should instead check if nextPos is on barrier or below bottom, (elem nextPos birdPositions || belowBottom )
    })  
  MaybeT . (fmap Just) $ (dead .= True)

-- | Move Bird to next position (up or down) and set direction back to down
move :: Game -> Game
move g@Game { _bird = b } = g 
          & bird .~ (nextPosition g) 
          & dir .~ Down --sets the Direction back to Down

-- | Get next position of the bird
nextPosition :: Game -> Coord
nextPosition Game { _dir = d, _bird = a }
  | d == Up    = a 
                & _y %~ (\y -> (y + 2) )
  | d == Down  = a 
                & _y %~ (\y -> (y - 1) )
nextPosition _ = error "Birds can't be empty!"


-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g = if g ^. locked
  then g
  else g 
    & dir .~ d  
    & paused .~ False 
    & locked .~ True

-- | Initialize a paused game 
initGame :: IO Game
initGame = do
  (f :| fs) <- fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _bird  = V2 xm ym
        , _score  = 0
        , _dir    = Down
        , _dead   = False
        , _paused = True
        , _locked = False
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
