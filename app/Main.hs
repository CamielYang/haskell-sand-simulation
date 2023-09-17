module Main (main) where

import Data.Array
import Data.Array.IO
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width, height, offset, frames :: Int
initialTransX, initialTransY, pixelSize :: Float
width = 640
height = 480
offset = 100
frames = 1

pixelSize = 5

initialTransX = fromIntegral width / 2 - pixelSize / 2

initialTransY = fromIntegral height / 2 - pixelSize / 2

window :: Display
window = InWindow "Simulation Game" (width, height) (offset, offset)

background :: Color
background = black

data Material
  = Sand
  | Water
  | Empty
  deriving (Eq)

type Coord = (Int, Int)

type Cell = Material

type Grid = IOArray Coord Cell

data GameState = GameState
  { frame :: Int,
    grid :: Grid
  }

generateGrid :: IO Grid
generateGrid = do
  mutableGrid <- thaw initialGrid
  let _ = mutableGrid :: IOArray Coord Cell
  return mutableGrid
  where
    initialGrid = array ((0, 0), (99, 99)) [((x, y), Empty) | x <- [0 .. 99], y <- [0 .. 99]]

handleKeys :: Event -> GameState -> IO GameState
handleKeys _ gameState = return gameState

createPixel :: Color -> Coord -> Picture
createPixel color' (x, y) =
  color color' $
    scale 1 (-1) $
      translate (fromIntegral x * pixelSize - initialTransX) (fromIntegral y * pixelSize - initialTransY) $
        rectangleSolid pixelSize pixelSize

render :: GameState -> IO Picture
render (GameState t g) = do
  immutableGrid <- freeze g
  let _ = immutableGrid :: Array Coord Cell
  return (pictures [createMat (immutableGrid ! c) c | c <- indices immutableGrid, immutableGrid ! c /= Empty])
  where
    createMat :: Cell -> Coord -> Picture
    createMat m c = createPixel white c

update :: Float -> GameState -> IO GameState
update _ state@(GameState t g) = do
  writeArray g (10, 0) Sand
  return
    ( GameState
        { frame = if t + 1 <= frames then t + 1 else 1,
          grid = g
        }
    )

main :: IO ()
main = do
  grid <- generateGrid
  playIO window background frames GameState {frame = 1, grid = grid} render handleKeys update