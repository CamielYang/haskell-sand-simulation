module Particles.MovableSolid.Sand where

import Lib
import Types

updateSand :: Coord -> Grid -> Bool -> IO ()
updateSand c g u = do
  removeParticle g c u
  (pB, _) <- getParticle g (getDir Bottom c)
  (p2, _) <- getParticle g (getDir BottomLeft c)
  (p3, _) <- getParticle g (getDir BottomRight c)
  (pL, _) <- getParticle g (getDir Types.Left c)
  (pR, _) <- getParticle g (getDir Types.Right c)
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