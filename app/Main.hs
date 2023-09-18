module Main (main) where

import Data.Array
import Data.Array.IO
import Data.ByteString (pack)
import Data.Foldable (forM_)
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Prelude hiding (Left, Right)

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
sizeY = 100
sizeX = 200
sizeY' = 100 - 1
sizeX' = 200 - 1
pixelSize = 3
width = sizeX * pixelSize
height = sizeY * pixelSize
offset = 100
frames = 120

window :: Display
window = InWindow "Simulation Game" (width, height) (offset, offset)

background :: Color
background = black

data Particle
  = Sand
  | Water
  | Filled
  | Empty
  deriving (Eq)

type Coord = (Int, Int)

type Cell = (Particle, Bool)

type GridA a = a Coord Cell

type Grid = GridA IOArray

data Direction
  = TopLeft
  | Top
  | TopRight
  | Left
  | Center
  | Right
  | BottomLeft
  | Bottom
  | BottomRight

data GameState = GameState
  { frame :: Int,
    grid :: Grid,
    toggleUpdated :: Bool
  }

getDir :: Direction -> Coord -> Coord
getDir TopLeft (x, y) = (x - 1, y - 1)
getDir Top (x, y) = (x, y - 1)
getDir TopRight (x, y) = (x + 1, y - 1)
getDir Left (x, y) = (x - 1, y)
getDir Center c = c
getDir Right (x, y) = (x + 1, y)
getDir BottomLeft (x, y) = (x - 1, y + 1)
getDir Bottom (x, y) = (x, y + 1)
getDir BottomRight (x, y) = (x + 1, y + 1)

generateGrid :: IO Grid
generateGrid = do
  mutableGrid <- thaw initialGrid
  let _ = mutableGrid :: Grid
  return mutableGrid
  where
    initialGrid = array ((0, 0), (sizeX', sizeY')) [((x, y), (Empty, False)) | x <- [0 .. sizeX'], y <- [0 .. sizeY']]

translateMouse :: (Float, Float) -> Coord
translateMouse (x, y) = (translateX, translateY)
  where
    translateX, translateY :: Int
    translateX = round (x / fromIntegral pixelSize + (fromIntegral sizeX / 2))
    translateY = round (-y / fromIntegral pixelSize + (fromIntegral sizeY / 2))

handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (MouseButton LeftButton) Down _ c) gameState@(GameState _ g _) = do
  createCell g (translateMouse c) (Water, False)
  return gameState
handleKeys _ gameState = return gameState

getParticleFromCell :: Cell -> Particle
getParticleFromCell (p, _) = p

getBoolFromCell :: Cell -> Bool
getBoolFromCell (_, b) = b

inBound :: Coord -> Bool
inBound (x, y) = x <= sizeX' && x >= 0 && y <= sizeY' && y >= 0

getParticle :: Grid -> Coord -> IO Cell
getParticle g c
  | inBound c = readArray g c
  | otherwise = return (Filled, False)

generateBitmap :: Array Coord Cell -> Picture
generateBitmap grid = byteStringToBitmap createPixelsArray
  where
    scaleBitmap = scale (fromIntegral pixelSize) (-(fromIntegral pixelSize))
    createPixelsArray = concat [createMat (getParticleFromCell (grid ! (x, y))) | y <- [0 .. sizeY'], x <- [0 .. sizeX']]
    byteStringToBitmap pixelArray = scaleBitmap $ bitmapOfByteString sizeX sizeY (BitmapFormat BottomToTop PxRGBA) (pack pixelArray) True
    createMat :: Particle -> [Word8]
    createMat m
      | m == Sand = [218, 211, 165, 255]
      | m == Water = [5, 138, 189, 255]
      | otherwise = [255, 255, 255, 255]

render :: GameState -> IO Picture
render (GameState _ g _) = do
  immutableGrid <- freeze g
  let _ = immutableGrid :: GridA Array
  return (generateBitmap immutableGrid)

createCell :: Grid -> Coord -> Cell -> IO ()
createCell grid coord cell
  | inBound coord = writeArray grid coord cell
  | otherwise = return ()

removeParticle :: Grid -> Coord -> Bool -> IO ()
removeParticle g c u = createCell g c (Empty, u)

updateSand :: Coord -> Grid -> Bool -> IO ()
updateSand c g u = do
  removeParticle g c u
  (p1, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir BottomLeft c)
  (p3, _) <- getParticle g (getDir BottomRight c)

  let action
        | p1 == Empty = createCell g (getDir Bottom c) (Sand, not u)
        | p2 == Empty = createCell g (getDir BottomLeft c) (Sand, not u)
        | p3 == Empty = createCell g (getDir BottomRight c) (Sand, not u)
        | otherwise = createCell g c (Sand, not u)
  action

updateWater :: Coord -> Grid -> Bool -> IO ()
updateWater c g u = do
  removeParticle g c u
  (p1, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir Left c)
  (p3, _) <- getParticle g (getDir Right c)

  let action
        | p1 == Empty = createCell g (getDir Bottom c) (Water, not u)
        | p2 == Empty = createCell g (getDir Left c) (Water, not u)
        | p3 == Empty = createCell g (getDir Right c) (Water, not u)
        | otherwise = createCell g c (Water, not u)
  action

update :: Float -> GameState -> IO GameState
update _ (GameState t g u) = do
  forM_ [0 .. sizeY'] $ \y -> do
    forM_ [0 .. sizeX'] $ \x -> do
      particle <- readArray g (x, y)
      let action
            | particle == (Sand, u) = updateSand (x, y) g u
            | particle == (Water, u) = updateWater (x, y) g u
            | otherwise = return ()
      action

  if t `mod` 1 == 0
    then do
      createCell g (10, 10) (Sand, False)
      createCell g (20, 10) (Sand, False)
      createCell g (30, 10) (Sand, False)
      createCell g (40, 10) (Sand, False)
      createCell g (50, 10) (Sand, False)
      createCell g (60, 10) (Sand, False)
    else return ()

  if t `mod` 5 == 0
    then do
      createCell g (70, 10) (Water, False)
      createCell g (80, 10) (Water, False)
      createCell g (90, 10) (Water, False)
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
  playIO
    window
    background
    frames
    GameState {frame = 1, grid = grid, toggleUpdated = False}
    render
    handleKeys
    update