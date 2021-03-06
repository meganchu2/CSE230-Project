{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module FlappyBird
  ( restart,
    initGame, 
    step,
    maybeDie,
    isCoordOnAnyBarrier,
    move,
    removeOldBarriers,
    replenishBarriers,
    moveBarriers,
    updateScore,
    nextPosition,
    turn,
    getBarrier,
    getBarriers,
    Game(..), 
    Direction(..), 
    dead, 
    score, 
    bird, 
    height, 
    width,
    groundLevel,
    restartReplenish,
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
  maybeDie <|> MaybeT (Just <$> modify move)

-- | Possibly die if next position is either on a barrier cell or above below grid (TODO)
maybeDie :: MaybeT (State Game) ()
maybeDie = do
  MaybeT . (fmap guard) $ (do 
    { 
      nextPos@(V2 x y) <- (nextPosition <$> get);      --get next position of bird
      birdPosition <- (use bird);
      barriers <- (use barriers);
      return $ (y < groundLevel) || (isCoordOnAnyBarrier barriers nextPos)        --check if bird Coord is on barrier or on floor
    })  
  MaybeT . (fmap Just) $ (dead .= True)

-- | returns true if coordinate is on any of the barriers (to determine whether game ends)
isCoordOnAnyBarrier :: Barriers -> Coord -> Bool
isCoordOnAnyBarrier barriers c = any isCoordOnBarrier barriers
    where isCoordOnBarrier barrier = c `elem` barrier

-- | Move Bird to next position (up or down) and set direction back to down
move :: Game -> Game
move g@Game { _bird = b, _barriers = bs, _barrierGen = bsgen } = g 
          & bird .~ (nextPosition g) 
          & barriers .~ (moveBarriers bs'' 1)
          & barrierGen .~ bsgen'
          & dir .~ Down --sets the Direction back to Down
          & score .~ (updateScore g)
  where bs' = removeOldBarriers bs
        (bs'', bsgen') = replenishBarriers bs' bsgen

-- | Remove old barriers and return the remaining barriers
removeOldBarriers :: Barriers -> Barriers
removeOldBarriers bs = bs'
  where bs' = filter (\(c:_) -> c ^. _x >= 0) bs

-- | Replenish barriers from the generator list
replenishBarriers :: Barriers -> [Int] -> (Barriers, [Int])
replenishBarriers bs bsgen = (bs', bsgen')
  where newBsNum = barrierNum - length bs
        (newbs, bsgen') = splitAt newBsNum bsgen
        lastX = (\(c:_) -> c ^. _x) $ last bs
        xs = [lastX + i * barrierInterval | i <- [1..newBsNum]]
        bs' = bs ++ (getBarriers xs newbs)

-- | Move every coordinate in every barrier an indicated amount left (i.e. x = x-i)
moveBarriers :: Barriers -> Int -> Barriers
moveBarriers barriers i = map moveBarrier barriers
  where moveBarrier barrier = map moveCoordinate barrier
        moveCoordinate (V2 x y) = (V2 (x-i) y)

-- | Increment the score when the bird passes a barrier
updateScore :: Game -> Int
updateScore g@Game {_bird = b, _barriers = bs, _score = s}
  | b ^. _x `elem` bsXs = s + 1
  | otherwise = s
  where bsXs = map (\((V2 x _):_) -> x) bs

-- | Get next position of the bird
nextPosition :: Game -> Coord
nextPosition Game { _dir = d, _bird = a }
  | d == Up    = a 
                & _y %~ (\y -> min (y + 4) height)
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


restart :: Game -> Game
restart g@Game { _barrierGen = bsgen } = 
  g
    & bird .~ V2 xm ym
    & score .~ 0
    & dir .~ Down
    & dead .~ False
    & paused .~ True
    & locked .~ False
    & barrierGen .~ bsgen'
    & barriers .~ bs''
--    & barrierGen .~ bg
    where
      xm = width `div` 4
      ym = height `div` 2
      (bs'', bsgen') = restartReplenish bsgen

-- | Replenish barriers from the generator list
restartReplenish :: [Int] -> (Barriers, [Int])
restartReplenish bsgen = (bs', bsgen')
  where (newbs, bsgen') = splitAt barrierNum bsgen
        xs = [(width `div` 4) + i * barrierInterval | i <- [1..barrierNum]]
        bs' = getBarriers xs newbs




-- | Initialize a paused game 
initGame :: IO Game
initGame = do
  b <- randomRs (barrierOpeningLo, barrierOpeningHi) <$> newStdGen
  let xm = width `div` 4
      ym = height `div` 2
      (bs, bg) = splitAt barrierNum b
      g  = Game
        { _bird  = V2 xm ym
        , _score  = 0
        , _dir    = Down
        , _dead   = False
        , _paused = True
        , _locked = False
        , _barriers = getBarriers [xm + barrierInterval * i | i <- [1..barrierNum]] bs
        , _barrierGen = bg
        }
  return g

-- | Generate a single barrier
getBarrier :: Int -> Int -> Barrier
getBarrier x y = [V2 x i | i <- [groundLevel..height], i < y - barrierOpeningWidth || i > y + barrierOpeningWidth]

-- | Generate barriers
getBarriers :: [Int] -> [Int] -> Barriers
getBarriers (x:xs) (y:ys) = (getBarrier x y) : (getBarriers xs ys)
getBarriers _ _ = []
