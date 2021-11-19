{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module FlappyBird
  ( initGame, 
    step,
    turn, 
    Game(..), 
    Direction(..), 
    dead, 
    score, 
    bird, 
    height, 
    width,
    barriers
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
import System.Random (Random(..), newStdGen)
import Linear.V2 (V2(..), _x, _y)

import Constants
import FlappyBirdTypes

makeLenses ''Game

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  MaybeT $ guard . not <$> orM [use paused, use dead] -- Make sure the game isn't paused or over
  MaybeT . (fmap Just) $ (locked .= False) -- Unlock from last directional turn
  modifying score (+ 1)
  maybeDie <|> MaybeT (Just <$> modify move)

-- | Possibly die if next position is either on a barrier cell or above below grid (TODO)
maybeDie :: MaybeT (State Game) ()
maybeDie = do
  MaybeT . (fmap guard) $ (do 
    { 
      nextPos@(V2 x y) <- (nextPosition <$> get);      --get next position of bird
      birdPosition <- (use bird);
      barriers <- (use barriers);
      return (isCoordOnAnyBarrier barriers nextPos)        --check if bird Coord is on barrier
                                                           --TODO: check if bird is on/below bottom of grid
    })  
  MaybeT . (fmap Just) $ (dead .= True)

-- | returns true if coordinate is on any of the barriers (to determine whether game ends)
isCoordOnAnyBarrier :: Barriers -> Coord -> Bool
isCoordOnAnyBarrier barriers c = any isCoordOnBarrier barriers
    where isCoordOnBarrier barrier = c `elem` barrier

-- | Move Bird to next position (up or down) and set direction back to down
move :: Game -> Game
move g@Game { _bird = b, _barriers = x } = g 
          & bird .~ (nextPosition g) 
          & barriers .~ (moveBarriers x)
          & dir .~ Down --sets the Direction back to Down

-- | Move every coordinate in every barrier one step left (i.e. x = x-1)
moveBarriers :: Barriers -> Barriers
moveBarriers barriers = map moveBarrier barriers
  where moveBarrier barrier = map moveCoordinate barrier
        moveCoordinate (V2 x y) = (V2 (x-1) y)


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
        , _barriers =  [[ V2 (xm+20) (ym-i) | i <- [5..20] ]]  
                                --TODO: add Game properties to know x/y coordinates of border of grid, create barrier based on this coordinate
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
