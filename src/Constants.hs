module Constants where

height, width :: Int
height = 25
width = 50

barrierInterval, barrierOpeningWidth, barrierNum :: Int
barrierInterval = 20
barrierOpeningWidth = 5
barrierNum = 10

barrierOpeningLo, barrierOpeningHi :: Int
barrierOpeningLo = getLo height 5
barrierOpeningHi = getHi height barrierOpeningLo

getLo :: Int -> Int -> Int
getLo a b = div a b
getHi :: Int -> Int -> Int
getHi a b = a - b

gameSpeed :: Int
gameSpeed = 100000
