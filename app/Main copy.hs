module Main (main) where

import Data.Array.MArray
import Data.Massiv.Array (Manifest (newMArray))
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
initialTransX, initialTransY, pixelSize :: Float
width = 640
height = 480
offset = 100

pixelSize = 5

initialTransX = fromIntegral width / 2 - pixelSize / 2

initialTransY = fromIntegral height / 2 - pixelSize / 2

frames = 10

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

type Cell = (Coord, Material)

type Cells = [Cell]

data GameState = GameState
  { index :: Int,
    grid :: Cells
  }

initialState :: GameState
initialState = GameState {index = 0, grid = [((x, y), Empty) | y <- [0 .. 9], x <- [0 .. 9]]}

isEmptyCell :: (Int, Int) -> Cells -> Bool
isEmptyCell _ [] = True
isEmptyCell x ((c, _) : cs)
  | x == c = False
  | otherwise = isEmptyCell x cs

handleKeys :: Event -> GameState -> GameState
handleKeys _ (GameState _ g) = GameState {index = 0, grid = g}

createPixel :: Color -> Coord -> Picture
createPixel color' (x, y) =
  color color' $
    scale 1 (-1) $
      translate (fromIntegral x * pixelSize - initialTransX) (fromIntegral y * pixelSize - initialTransY) $
        rectangleSolid pixelSize pixelSize

determinePixel (x, y) i
  | ((even (round x) && odd (round y)) || (odd (round x) && even (round y))) = createPixel colorOne (x, y)
  | otherwise = createPixel colorTwo (x, y)
  where
    colorOne = [red, green, blue, yellow, black] !! i
    colorTwo = [blue, red, green, black, yellow] !! i
    createPixel color' (x, y) =
      color color' $
        scale 1 (-1) $
          translate (x * pixelSize - initialTransX) (y * pixelSize - initialTransY) $
            rectangleSolid pixelSize pixelSize

render :: GameState -> Picture
render (GameState t g) = pictures ([createMat c | c@(_, m) <- g, m /= Empty])
  where
    createMat :: Cell -> Picture
    createMat (c, m)
      | m == Sand = createPixel orange c
      | otherwise = createPixel blue c

update :: Float -> GameState -> GameState
update _ gameState = toggleState gameState

updateSand :: Cell -> Cells -> Cell
updateSand c@((x, y), m) g
  | y == 50 = c
  | isEmptyCell (x, y + 1) g = ((x, y + 1), m)
  | isEmptyCell (x - 1, y + 1) g = ((x - 1, y + 1), m)
  | isEmptyCell (x + 1, y + 1) g = ((x + 1, y + 1), m)
  | otherwise = ((x, y), m)

updateWater :: Cell -> Cells -> Cell
updateWater c@((x, y), m) g
  | y == 50 = c
  | isEmptyCell (x, y + 1) g = ((x, y + 1), m)
  | isEmptyCell (x - 1, y) g = ((x - 1, y), m)
  | isEmptyCell (x + 1, y) g = ((x + 1, y), m)
  | otherwise = ((x, y), m)

toggleState :: GameState -> GameState
toggleState (GameState t g) = GameState {index = if t + 1 < 5 then t + 1 else 0, grid = createCell Sand : createCell Water : updateGrid}
  where
    mArray = newMArray
    updateGrid :: Cells
    updateGrid = [updateCell c g | c@(_, m) <- g, m /= Empty]
    updateCell :: Cell -> Cells -> Cell
    updateCell c@((x, y), m) g
      | m == Sand = updateSand c g
      | otherwise = updateWater c g
    createCell :: Material -> Cell
    createCell m
      | t == 0 && m == Sand = ((50, 1), Sand)
      | m == Water = ((70 + t, 1), Water)
      | otherwise = ((0, 0), Empty)

main :: IO ()
main = play window background frames initialState render handleKeys update