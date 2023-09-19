module Main (main) where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.ByteString (pack)
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
offset = 100
frames = 60
pixelSize = 3
sizeY = 100
sizeX = 200
width = sizeX * pixelSize
height = sizeY * pixelSize
sizeY' = sizeY - 1
sizeX' = sizeX - 1

window :: Display
window = InWindow "Simulation Game" (width, height) (offset, offset)

background :: Color
background = black

data Particle
  = Sand
  | Water
  | Stone
  | Wood
  | Grass
  | Empty
  deriving (Eq, Show, Enum, Bounded)

allParticles :: [Particle]
allParticles = [(minBound :: Particle) ..]

data ParticleData = ParticleNew
  { pVelocity :: Int,
    pColor :: [Word8],
    pDirections :: [Direction]
  }

pd :: Particle -> ParticleData
pd Sand =
  ParticleNew
    { pVelocity = 10,
      pColor = [218, 211, 165, 255],
      pDirections = [Bottom, BottomLeft, BottomRight]
    }
pd Stone =
  ParticleNew
    { pVelocity = 10,
      pColor = [185, 185, 186, 255],
      pDirections = []
    }
pd Wood =
  ParticleNew
    { pVelocity = 10,
      pColor = [136, 118, 71, 255],
      pDirections = []
    }
pd Grass =
  ParticleNew
    { pVelocity = 10,
      pColor = [101, 159, 72, 255],
      pDirections = []
    }
pd Water =
  ParticleNew
    { pVelocity = 10,
      pColor = [5, 138, 189, 255],
      pDirections = [Bottom, BottomLeft, BottomRight, Left, Right]
    }
pd _ =
  ParticleNew
    { pVelocity = 0,
      pColor = [255, 255, 255, 255],
      pDirections = []
    }

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
    toggleUpdated :: Bool,
    mouseDown :: Bool,
    mousePosition :: Coord,
    selectedParticle :: Particle
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
handleKeys (EventKey (MouseButton LeftButton) Down _ _) gameState = do
  return gameState {mouseDown = True}
handleKeys (EventKey (MouseButton LeftButton) Up _ _) gameState = do
  return gameState {mouseDown = False}
handleKeys (EventMotion c) gameState = do
  return gameState {mousePosition = translateMouse c}
handleKeys (EventKey (Char 'd') Down _ _) gameState = do
  return
    gameState
      { selectedParticle =
          if last allParticles == selectedParticle gameState
            then head allParticles
            else succ $ selectedParticle gameState
      }
handleKeys (EventKey (Char 'a') Down _ _) gameState = do
  return
    gameState
      { selectedParticle =
          if head allParticles == selectedParticle gameState
            then last allParticles
            else pred $ selectedParticle gameState
      }
handleKeys _ gameState = return gameState

getParticleFromCell :: Cell -> Particle
getParticleFromCell (p, _) = p

-- getBoolFromCell :: Cell -> Bool
-- getBoolFromCell (_, b) = b

inBound :: Coord -> Bool
inBound (x, y) = x <= sizeX' && x >= 0 && y <= sizeY' && y >= 0

getParticle :: Grid -> Coord -> IO Cell
getParticle g c
  | inBound c = readArray g c
  | otherwise = return (Sand, False)

generateBitmap :: Array Coord Cell -> Picture
generateBitmap grid = byteStringToBitmap createPixelsArray
  where
    scaleBitmap = scale (fromIntegral pixelSize) (-(fromIntegral pixelSize))
    createPixelsArray = concat [createPixel (getParticleFromCell (grid ! (x, y))) | y <- [0 .. sizeY'], x <- [0 .. sizeX']]
    byteStringToBitmap pixelArray = scaleBitmap $ bitmapOfByteString sizeX sizeY (BitmapFormat BottomToTop PxRGBA) (pack pixelArray) True
    createPixel :: Particle -> [Word8]
    createPixel p = pColor $ pd p

render :: GameState -> IO Picture
render gameState = do
  immutableGrid <- freeze (grid gameState)
  let _ = immutableGrid :: GridA Array
  return
    ( pictures
        [ generateBitmap immutableGrid,
          drawText
        ]
    )
  where
    drawText = translate (-(fromIntegral width / 2 - 10)) (fromIntegral height / 2 - 20) $ scale 0.1 0.1 $ text (show $ selectedParticle gameState)

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
  (p4, _) <- getParticle g (getDir Left c)
  (p5, _) <- getParticle g (getDir Right c)

  let action
        | p1 == Empty = createCell g (getDir Bottom c) (Sand, not u)
        | p2 == Empty && p4 == Empty = createCell g (getDir BottomLeft c) (Sand, not u)
        | p3 == Empty && p5 == Empty = createCell g (getDir BottomRight c) (Sand, not u)
        | otherwise = createCell g c (Sand, not u)
  action

updateWater :: Coord -> Grid -> Bool -> IO ()
updateWater c g u = do
  removeParticle g c u
  (p1, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir BottomLeft c)
  (p3, _) <- getParticle g (getDir BottomRight c)
  (p4, _) <- getParticle g (getDir Left c)
  (p5, _) <- getParticle g (getDir Right c)

  let action
        | p1 == Empty = createCell g (getDir Bottom c) (Water, not u)
        | p2 == Empty && p4 == Empty = createCell g (getDir BottomLeft c) (Water, not u)
        | p3 == Empty && p5 == Empty = createCell g (getDir BottomRight c) (Water, not u)
        | p4 == Empty = createCell g (getDir Left c) (Water, not u)
        | p5 == Empty = createCell g (getDir Right c) (Water, not u)
        | otherwise = createCell g c (Water, not u)
  action

update :: Float -> GameState -> IO GameState
update _ gameState@(GameState t g u md mp sp) = do
  forM_ [0 .. sizeY'] $ \y -> do
    forM_ [0 .. sizeX'] $ \x -> do
      particle <- readArray g (x, y)
      let action
            | particle == (Sand, u) = updateSand (x, y) g u
            | particle == (Water, u) = updateWater (x, y) g u
            | otherwise = return ()
      action

  when md $ do
    createCell g (getDir Center mp) (sp, not u)
    createCell g (getDir Top mp) (sp, not u)
    createCell g (getDir Bottom mp) (sp, not u)
    createCell g (getDir Left mp) (sp, not u)
    createCell g (getDir Right mp) (sp, not u)

  -- when (t `mod` 2 == 0) $ do
  --   createCell g (10, 10) (Sand, not u)
  --   createCell g (20, 10) (Sand, not u)
  --   createCell g (30, 10) (Sand, not u)
  --   createCell g (40, 10) (Sand, not u)
  --   createCell g (50, 10) (Sand, not u)
  --   createCell g (60, 10) (Sand, not u)

  -- when (t `mod` 5 == 0) $ do
  --   createCell g (70, 10) (Water, not u)
  --   createCell g (80, 10) (Water, not u)
  --   createCell g (90, 10) (Water, not u)
  return
    gameState
      { frame = if t + 1 <= frames then t + 1 else 1,
        toggleUpdated = not u
      }

main :: IO ()
main = do
  grid <- generateGrid
  playIO
    window
    background
    frames
    GameState
      { frame = 1,
        grid = grid,
        toggleUpdated = False,
        mouseDown = False,
        mousePosition = (0, 0),
        selectedParticle = Sand
      }
    render
    handleKeys
    update