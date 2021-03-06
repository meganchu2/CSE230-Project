{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import FlappyBird

import Brick
  ( App(..), 
    AttrMap, 
    BrickEvent(..), 
    EventM, 
    Next, 
    Widget, 
    customMain, 
    neverShowCursor, 
    continue, 
    halt, 
    hLimit, vLimit, 
    vBox, hBox, 
    padRight, padLeft, padTop, padAll, Padding(..), 
    withBorderStyle, 
    str, 
    attrMap, withAttr, emptyWidget, AttrName, on, fg, 
    (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), makeLenses)
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)

import Constants
import UITypes

data View
  = GameView { g :: Game }
  | GameOverView { g :: Game }


-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do {
    writeBChan chan Tick;
    threadDelay gameSpeed;
  }
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue (step g)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue (turn Up g)
--handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue (turn Up g)
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = if (g ^. dead) then continue (restart g) else continue (turn Up g)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue (step g)

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  if (g ^. dead)
    then [drawGameOverScreen g] ++ [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]
    else [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawGameOverScreen :: Game -> Widget Name
drawGameOverScreen g =  withAttr restartAttr $ C.centerLayer $ str "Press Space to Restart"

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Flappy Bird")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == (g ^. bird)                        = Bird
      | c `elem` (concat (g ^. barriers))       = Barrier
      | c ^. _y >= groundLevel                 = Sky
      | c ^. _y == groundLevel - 1 = Grass1 --if (c ^. _x %2 == 0 && )
      | otherwise = Ground

drawCell :: Cell -> Widget Name
drawCell Bird = withAttr birdAttr cw
drawCell Barrier = withAttr barrierAttr cw
drawCell Empty = withAttr emptyAttr cw
drawCell Sky = withAttr skyAttr cw
drawCell Ground = withAttr groundAttr cw
drawCell Grass1 = withAttr grass1Attr cw

-- comment
cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (birdAttr, V.blue `on` birdColor),
    (barrierAttr, V.red `on` barColor),  
    (gameOverAttr, fg V.red `V.withStyle` V.bold),
    (skyAttr, V.red `on` skyColor),
    (groundAttr, V.red `on` groundColor),
    (grass1Attr, V.red `on` grass1Color),
    (grass2Attr, V.red `on` grass2Color),
    (restartAttr, (V.blue `on` V.brightYellow) `V.withStyle` V.bold)
  ]

restartAttr :: AttrName
restartAttr = "restart"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

birdAttr, barrierAttr, emptyAttr, skyAttr, groundAttr :: AttrName
birdAttr = "birdAttr"
barrierAttr = "barrierAttr"
emptyAttr = "emptyAttr"
skyAttr = "skyAttr"
groundAttr = "groundAttr"
grass1Attr = "grass1Attr"
grass2Attr = "grass2Attr"

skyColor, barColor, birdColor, cloudColor, groundColor, grass1Color, grass2Color :: V.Color
skyColor = V.rgbColor 109 190 199
barColor = V.rgbColor 62 179 62
birdColor = V.rgbColor 247 212 69
cloudColor = V.rgbColor 247 247 247
groundColor = V.rgbColor 242 229 177
grass1Color = V.rgbColor 188 242 177
grass2Color = V.rgbColor 79 173 61
