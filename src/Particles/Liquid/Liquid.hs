module Particles.Liquid.Liquid where

import Data.Array
import Debug.Trace
import Lib
import Types

getFurthestDispersion :: ImmutableGrid -> [Coord] -> Coord
getFurthestDispersion _ [] = (-1, -1)
getFurthestDispersion g [x]
  | inBound x && getParticleFromCell (g ! x) == Empty = x
  | otherwise = (-1, -1)
getFurthestDispersion g [x, y]
  | inBound y && getParticleFromCell (g ! y) == Empty = y
  | otherwise = getFurthestDispersion g [x]
getFurthestDispersion g (x : y : xs)
  | inBound y && getParticleFromCell (g ! y) == Empty = getFurthestDispersion g (y : xs)
  | otherwise = x

updateLiquid :: ParticleType -> Coord -> GameState -> Bool -> IO ()
updateLiquid p c@(x, y) gs u = do
  let g = grid gs
  let ig = immutableGrid gs

  removeParticle g c u
  (pB, _) <- getParticle g (getDir Bottom c)
  let lLines = [(x', y) | x' <- reverse [x - 5 .. x]]
  let lCoord = getFurthestDispersion ig lLines
  (pL, _) <- getParticle g lCoord

  let rLines = [(x', y) | x' <- [x .. x + 5]]
  let rCoord = getFurthestDispersion ig rLines
  (pR, _) <- getParticle g rCoord

  dirChange <- randInt 0 1

  let action
        | pB == Empty = createCell g (getDir Bottom c) (p, not u)
        | pL == Empty && (pR /= Empty || dirChange == 0) = createCell g lCoord (p, not u)
        | pR == Empty = createCell g rCoord (p, not u)
        | otherwise = createCell g c (p, not u)
  action