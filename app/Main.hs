module Main (main) where

import Data.Array
import Data.Array.IO
import Data.ByteString (ByteString, pack)
import Data.Foldable (forM_)
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width, height, offset, frames, sizeY, sizeX :: Int
initialTransX, initialTransY, pixelSize :: Float
width = 640
height = 480
offset = 100
frames = 120
sizeY = 100
sizeX = 200

pixelSize = 5

initialTransX = fromIntegral width / 2 - pixelSize / 2

initialTransY = fromIntegral height / 2 - pixelSize / 2

window :: Display
window = InWindow "Simulation Game" (width, height) (offset, offset)

background :: Color
background = black

data Particle
  = Sand
  | Water
  | Empty
  deriving (Eq)

type Coord = (Int, Int)

type Cell = (Particle, Bool)

type GridA a = a Coord Cell

type Grid = GridA IOArray

data GameState = GameState
  { frame :: Int,
    grid :: Grid,
    toggleUpdated :: Bool
  }

generateGrid :: IO Grid
generateGrid = do
  mutableGrid <- thaw initialGrid
  let _ = mutableGrid :: Grid
  return mutableGrid
  where
    initialGrid = array ((0, 0), (sizeX, sizeY)) [((x, y), (Empty, False)) | x <- [0 .. sizeX], y <- [0 .. sizeY]]

handleKeys :: Event -> GameState -> IO GameState
handleKeys _ gameState = return gameState

createPixel :: Color -> Coord -> Picture
createPixel color' (x, y) =
  color color' $
    scale 1 (-1) $
      translate (fromIntegral x * pixelSize - initialTransX) (fromIntegral y * pixelSize - initialTransY) $
        rectangleSolid pixelSize pixelSize

getParticleFromCell :: Cell -> Particle
getParticleFromCell (p, _) = p

getBoolFromCell :: Cell -> Bool
getBoolFromCell (_, b) = b

getParticle :: Grid -> Coord -> IO Cell
getParticle g c@(x, y)
  | x <= sizeX && x >= 0 && y <= sizeY && y >= 0 = readArray g c
  | otherwise = return (Sand, False)

purple :: [Word8]
purple = [128, 0, 128, 64]

-- bitmapData :: ByteString
bitmapData = take 8 (cycle purple)

render :: GameState -> IO Picture
render (GameState t g _) = do
  immutableGrid <- freeze g
  let _ = immutableGrid :: GridA Array
  let test = concat [createMat (getParticleFromCell (immutableGrid ! (x, y))) | y <- [1 .. sizeY], x <- [1 .. sizeX]]
  return (scale 5 (-5) $ bitmapOfByteString sizeX sizeY (BitmapFormat BottomToTop PxRGBA) (pack test) True)
  where
    createMat :: Particle -> [Word8]
    createMat m
      | m == Sand = [254, 254, 254, 255]
      | m == Water = [254, 254, 254, 150]
      | otherwise = [254, 254, 254, 100]

createCell :: Grid -> Cell -> Coord -> IO ()
createCell g p c@(x, y)
  | x <= sizeX && x >= 0 && y <= sizeY && y >= 0 = writeArray g c p
  | otherwise = return ()

removeParticle :: Grid -> Coord -> Bool -> IO ()
removeParticle g c b = createCell g (Empty, b) c

updateSand :: Coord -> Grid -> Bool -> IO ()
updateSand (x, y) g b = do
  removeParticle g (x, y) b
  (p1, _) <- getParticle g (x, y + 1)
  (p2, _) <- getParticle g (x - 1, y + 1)
  (p3, _) <- getParticle g (x + 1, y + 1)

  let action
        | p1 == Empty = createCell g (Sand, not b) (x, y + 1)
        | p2 == Empty = createCell g (Sand, not b) (x - 1, y + 1)
        | p3 == Empty = createCell g (Sand, not b) (x + 1, y + 1)
        | otherwise = createCell g (Sand, not b) (x, y)
  action

updateWater :: Coord -> Grid -> Bool -> IO ()
updateWater (x, y) g b = do
  removeParticle g (x, y) b
  (p1, _) <- getParticle g (x, y + 1)
  (p2, _) <- getParticle g (x - 1, y)
  (p3, _) <- getParticle g (x + 1, y)

  let action
        | p1 == Empty = createCell g (Water, not b) (x, y + 1)
        | p2 == Empty = createCell g (Water, not b) (x - 1, y)
        | p3 == Empty = createCell g (Water, not b) (x + 1, y)
        | otherwise = createCell g (Water, not b) (x, y)
  action

update :: Float -> GameState -> IO GameState
update _ (GameState t g u) = do
  forM_ [0 .. sizeY] $ \y -> do
    forM_ [0 .. sizeX] $ \x -> do
      particle <- readArray g (x, y)
      let action
            | particle == (Sand, u) = updateSand (x, y) g u
            | particle == (Water, u) = updateWater (x, y) g u
            | otherwise = return ()
      action

  if t `mod` 1 == 0
    then do
      createCell g (Sand, False) (10, 10)
      createCell g (Sand, False) (20, 10)
      createCell g (Sand, False) (30, 10)
      createCell g (Sand, False) (40, 10)
      createCell g (Sand, False) (50, 10)
      createCell g (Sand, False) (60, 10)
    else return ()

  if t `mod` 5 == 0
    then do
      createCell g (Water, False) (70, 10)
      createCell g (Water, False) (80, 10)
      createCell g (Water, False) (90, 10)
    else return ()
  return
    ( GameState
        { frame = if t + 1 <= frames then t + 1 else 1,
          grid = g,
          toggleUpdated = not u
        }
    )

main :: IO ()
main = do
  grid <- generateGrid
  playIO window background frames GameState {frame = 1, grid = grid, toggleUpdated = False} render handleKeys update