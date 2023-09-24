module Gloss.Game where

import Config
import Control.Monad
import Data.Array
import Data.Array.IO
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Raster.Field (makePicture)
import Lib
import Particles.Particle
import Types
import Prelude hiding (Left, Right)

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

render :: GameState -> IO Picture
render gameState = do
  let ig = immutableGrid gameState
  let makePixel (x, y) = pColor $ pd (getParticleFromCell (ig ! (pointToCoord x sizeX, pointToCoord y sizeY)))

  return
    ( pictures
        [ scaleGrid $ makePicture sizeX sizeY 1 1 makePixel,
          drawText
        ]
    )
  where
    scaleGrid = scale (fromIntegral pixelSize) (-(fromIntegral pixelSize))
    drawText = translate (-(fromIntegral width / 2 - 10)) (fromIntegral height / 2 - 20) $ scale 0.1 0.1 $ text (show $ selectedParticle gameState)

update :: Float -> GameState -> IO GameState
update _ gameState = do
  immutableGrid <- freeze (grid gameState)
  let _ = immutableGrid :: GridA Array

  let t = frame gameState
  let g = grid gameState
  let u = toggleUpdated gameState
  let md = mouseDown gameState
  let mp@(xmp, ymp) = mousePosition gameState
  let (xpmp, ypmp) = prevMousePosition gameState
  let sp = selectedParticle gameState

  forM_ [0 .. sizeY'] $ \y -> do
    forM_ shuffledX $ \x -> do
      particle <- readArray g (x, y)
      let action
            | particle == (Sand, u) = updateSand (x, y) g u
            | particle == (Water, u) = updateWater (x, y) gameState u
            | particle == (Acid, u) = updateAcid (x, y) gameState u
            | otherwise = return ()
      action

  when md $ do
    coords <- drawLineCoords xmp ymp xpmp ypmp
    forM_ [0 .. length coords - 1] $ \i -> do
      let center = coords !! i
      createCell g center (sp, not u)
      createCell g (getDir Top center) (sp, not u)
      createCell g (getDir Bottom center) (sp, not u)
      createCell g (getDir Left center) (sp, not u)
      createCell g (getDir Right center) (sp, not u)

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
        toggleUpdated = not u,
        prevMousePosition = mp,
        immutableGrid = immutableGrid
      }