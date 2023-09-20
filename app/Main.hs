module Main (main) where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.ByteString (pack)
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
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

data ParticleType
  = Sand
  | Water
  | Stone
  | Wood
  | Grass
  | Acid
  | Empty
  deriving (Eq, Show, Enum, Bounded)

allParticles :: [ParticleType]
allParticles = [(minBound :: ParticleType) ..]

data ParticleData = Particle
  { pFallRate :: Int,
    pColor :: [Word8]
  }

defaultParticle =
  Particle
    { pFallRate = 1,
      pColor = [255, 255, 255, 255]
    }

defaultStaticParticle =
  Particle
    { pFallRate = 0,
      pColor = [255, 255, 255, 255]
    }

pd :: ParticleType -> ParticleData
pd Sand = defaultParticle {pColor = [218, 211, 165, 255]}
pd Water = defaultParticle {pColor = [5, 138, 189, 255]}
pd Acid = defaultParticle {pColor = [114, 209, 68, 255]}
pd Stone = defaultStaticParticle {pColor = [185, 185, 186, 255]}
pd Wood = defaultStaticParticle {pColor = [136, 118, 71, 255]}
pd Grass = defaultStaticParticle {pColor = [101, 159, 72, 255]}
pd _ = defaultStaticParticle {pColor = [255, 255, 255, 255]}

type Coord = (Int, Int)

type Cell = (ParticleType, Bool)

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
    selectedParticle :: ParticleType
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

getParticleFromCell :: Cell -> ParticleType
getParticleFromCell (p, _) = p

-- getBoolFromCell :: Cell -> Bool
-- getBoolFromCell (_, b) = b

inBound :: Coord -> Bool
inBound (x, y) = x <= sizeX' && x >= 0 && y <= sizeY' && y >= 0

randInt :: Int -> Int -> IO Int
randInt x y = getStdRandom (randomR (x, y))

getParticle :: Grid -> Coord -> IO Cell
getParticle g c
  | inBound c = readArray g c
  | otherwise = return (Sand, False)

generateBitmap :: Array Coord Cell -> Picture
generateBitmap grid = byteStringToBitmap createPixelsArray
  where
    scaleBitmap = scale (fromIntegral pixelSize) (-(fromIntegral pixelSize))
    createPixelsArray = concat [createPixel (getParticleFromCell (grid ! (x, y))) | y <- [0 .. sizeY'], x <- [0 .. sizeX']]
    byteStringToBitmap pixelArray = scaleBitmap $ bitmapOfByteString sizeX sizeY (BitmapFormat BottomToTop PxRGBA) (pack pixelArray) False
    createPixel :: ParticleType -> [Word8]
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

isLiquid :: ParticleType -> Bool
isLiquid p
  | p == Water || p == Acid = True
  | otherwise = False

updateSand :: Coord -> Grid -> Bool -> IO ()
updateSand c g u = do
  removeParticle g c u
  (pB, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir BottomLeft c)
  (p3, _) <- getParticle g (getDir BottomRight c)
  (pL, _) <- getParticle g (getDir Left c)
  (pR, _) <- getParticle g (getDir Right c)
  dirChange <- randInt 0 1
  let pBL p = p2 == p && pL == p
  let pBR p = p3 == p && pR == p

  let plBL = isLiquid p2 && isLiquid pL
  let plBR = isLiquid p3 && isLiquid pR

  let action
        | pB == Empty = createCell g (getDir Bottom c) (Sand, not u)
        | pBL Empty && (not (pBR Empty) || dirChange == 0) = createCell g (getDir BottomLeft c) (Sand, not u)
        | pBR Empty = createCell g (getDir BottomRight c) (Sand, not u)
        | isLiquid pB = do
            createCell g (getDir Bottom c) (Sand, not u)
            createCell g c (pB, not u)
        | plBL && (not plBR || dirChange == 0) = createCell g (getDir BottomLeft c) (Sand, not u)
        | plBL = do
            createCell g (getDir BottomLeft c) (Sand, not u)
            createCell g c (p2, not u)
        | plBR = do
            createCell g (getDir BottomRight c) (Sand, not u)
            createCell g c (p3, not u)
        | otherwise = createCell g c (Sand, not u)
  action

updateWater :: Coord -> Grid -> Bool -> IO ()
updateWater c g u = do
  removeParticle g c u
  (pB, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir BottomLeft c)
  (p3, _) <- getParticle g (getDir BottomRight c)
  (pL, _) <- getParticle g (getDir Left c)
  (pR, _) <- getParticle g (getDir Right c)
  dirChange <- randInt 0 1
  let pBL = p2 == Empty && pL == Empty
  let pBR = p3 == Empty && pR == Empty

  let action
        | pB == Empty = createCell g (getDir Bottom c) (Water, not u)
        | pBL && (not pBR || dirChange == 0) = createCell g (getDir BottomLeft c) (Water, not u)
        | pBR = createCell g (getDir BottomRight c) (Water, not u)
        | pL == Empty && (pR /= Empty || dirChange == 0) = createCell g (getDir Left c) (Water, not u)
        | pR == Empty = createCell g (getDir Right c) (Water, not u)
        | otherwise = createCell g c (Water, not u)
  action

updateAcid :: Coord -> Grid -> Bool -> IO ()
updateAcid c g u = do
  removeParticle g c u
  (pB, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir BottomLeft c)
  (p3, _) <- getParticle g (getDir BottomRight c)
  (pL, _) <- getParticle g (getDir Left c)
  (pR, _) <- getParticle g (getDir Right c)

  dirChange <- randInt 0 1
  stoneChange <- randInt 0 300
  corrodeChange <- randInt 0 100

  let pBL = p2 == Empty && pL == Empty
  let pBR = p3 == Empty && pR == Empty

  let corrose p v = p /= Empty && p /= Acid && ((p == Stone && stoneChange < v) || (p /= Stone && corrodeChange < v))

  let corroseAction
        | inBound (getDir Bottom c) && corrose pB 1 = createCell g (getDir Bottom c) (Empty, not u)
        | inBound (getDir Left c) && corrose pL 2 = createCell g (getDir Left c) (Empty, not u)
        | inBound (getDir Right c) && corrose pR 3 = createCell g (getDir Right c) (Empty, not u)
        | otherwise = return ()

  let action
        | pB == Empty = createCell g (getDir Bottom c) (Acid, not u)
        | pBL && (not pBR || dirChange == 0) = createCell g (getDir BottomLeft c) (Acid, not u)
        | pBR = createCell g (getDir BottomRight c) (Acid, not u)
        | pL == Empty && (pR /= Empty || dirChange == 0) = createCell g (getDir Left c) (Acid, not u)
        | pR == Empty = createCell g (getDir Right c) (Acid, not u)
        | otherwise = createCell g c (Acid, not u)
  corroseAction
  action

update :: Float -> GameState -> IO GameState
update _ gameState@(GameState t g u md mp sp) = do
  forM_ [0 .. sizeY'] $ \y -> do
    forM_ (if u then [0 .. sizeX'] else reverse [0 .. sizeX']) $ \x -> do
      particle <- readArray g (x, y)
      let action
            | particle == (Sand, u) = updateSand (x, y) g u
            | particle == (Water, u) = updateWater (x, y) g u
            | particle == (Acid, u) = updateAcid (x, y) g u
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