module Particles.Liquid.Acid where

import Lib
import Particles.Liquid.Liquid
import Types

updateAcid :: Coord -> GameState -> Bool -> IO ()
updateAcid c gs u = do
  let g = grid gs

  (pB, _) <- getParticle g (getDir Bottom c)
  (pL, _) <- getParticle g (getDir Types.Left c)
  (pR, _) <- getParticle g (getDir Types.Right c)

  stoneChange <- randInt 0 300
  corrodeChange <- randInt 0 100

  let corrose p v = p /= Empty && p /= Acid && ((p == Stone && stoneChange < v) || (p /= Stone && corrodeChange < v))

  let corroseAction
        | inBound (getDir Bottom c) && corrose pB 1 = createCell g (getDir Bottom c) (Empty, not u)
        | inBound (getDir Types.Left c) && corrose pL 2 = createCell g (getDir Types.Left c) (Empty, not u)
        | inBound (getDir Types.Right c) && corrose pR 3 = createCell g (getDir Types.Right c) (Empty, not u)
        | otherwise = return ()

  corroseAction
  updateLiquid Acid c gs u