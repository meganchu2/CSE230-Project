module FlappyBirdTypes where
import Linear.V2 (V2(..), _x, _y)

data Game = Game
  { _bird   :: Bird         -- ^ bird as a coordinate
  , _barriers :: Barriers   -- barrier as a sequence of sequence of coordinates
  , _barrierGen :: [Int]    -- infinite list for generating future barriers
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

type Barrier = [Coord]
type Barriers = [Barrier]
