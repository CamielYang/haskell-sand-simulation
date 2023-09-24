module Particles.Liquid.Water where

import Particles.Liquid.Liquid
import Types

updateWater :: Coord -> GameState -> Bool -> IO ()
updateWater c gs u = do
  updateLiquid Water c gs u