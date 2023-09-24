module Config where

import Graphics.Gloss
import System.Random
import System.Random.Shuffle

width,
  height,
  offset,
  frames,
  sizeY,
  sizeX,
  sizeY',
  sizeX',
  pixelSize ::
    Int
offset = 100
frames = 60
pixelSize = 3
sizeY = 150
sizeX = 300
width = sizeX * pixelSize
height = sizeY * pixelSize
sizeY' = sizeY - 1
sizeX' = sizeX - 1

shuffledX :: [Int]
shuffledX = shuffle' [0 .. sizeX'] sizeX (mkStdGen 137)

background :: Color
background = black

window :: Display
window = InWindow "Sand Simulation" (width, height) (offset, offset)