module Constants where

height, width :: Int
height = 30
width = 50

barrierInterval, barrierOpeningWidth, barrierNum :: Int
barrierInterval = 20
barrierOpeningWidth = 5
barrierNum = 10

barrierOpeningLo, barrierOpeningHi :: Int
barrierOpeningLo = div height 5
barrierOpeningHi = height - barrierOpeningLo

gameSpeed :: Int
gameSpeed = 100000
