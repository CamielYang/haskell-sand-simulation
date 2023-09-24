module Main (main) where

import Config
import Data.Array.IO
-- import Debug.Trace

import Gloss.Game
import Graphics.Gloss.Interface.IO.Game
import Types

main :: IO ()
main = do
  grid <- generateGrid
  immutableGrid <- freeze grid
  playIO
    window
    background
    frames
    GameState
      { frame = 1,
        grid = grid,
        immutableGrid = immutableGrid,
        toggleUpdated = False,
        mouseDown = False,
        mousePosition = (0, 0),
        prevMousePosition = (0, 0),
        selectedParticle = Sand
      }
    render
    handleKeys
    update