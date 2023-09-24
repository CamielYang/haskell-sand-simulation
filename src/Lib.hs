module Lib where

import Config
import Data.Array.Base
import System.Random
import Types

getParticleFromCell :: Cell -> ParticleType
getParticleFromCell (p, _) = p

-- getBoolFromCell :: Cell -> Bool
-- getBoolFromCell (_, b) = b

inBound :: Coord -> Bool
inBound (x, y) = x <= sizeX' && x >= 0 && y <= sizeY' && y >= 0

randInt :: Int -> Int -> IO Int
randInt x y = getStdRandom (randomR (x, y))

getDir :: Direction -> Coord -> Coord
getDir TopLeft (x, y) = (x - 1, y - 1)
getDir Top (x, y) = (x, y - 1)
getDir TopRight (x, y) = (x + 1, y - 1)
getDir Types.Left (x, y) = (x - 1, y)
getDir Center c = c
getDir Types.Right (x, y) = (x + 1, y)
getDir BottomLeft (x, y) = (x - 1, y + 1)
getDir Bottom (x, y) = (x, y + 1)
getDir BottomRight (x, y) = (x + 1, y + 1)

getParticle :: Grid -> Coord -> IO Cell
getParticle g c
  | inBound c = readArray g c
  | otherwise = return (Sand, False)

createCell :: Grid -> Coord -> Cell -> IO ()
createCell grid coord cell
  | inBound coord = writeArray grid coord cell
  | otherwise = return ()

removeParticle :: Grid -> Coord -> Bool -> IO ()
removeParticle g c u = createCell g c (Empty, u)

drawLineCoords :: Int -> Int -> Int -> Int -> IO [Coord]
drawLineCoords x1 y1 x2 y2 = do
  let useX = abs (x1 - x2) > abs (y1 - y2)
  let isBigger = if useX then x1 < x2 else y1 < y2
  let xStart = if isBigger then x1 else x2
  let yStart = if isBigger then y1 else y2
  let xEnd = if isBigger then x2 else x1
  let yEnd = if isBigger then y2 else y1

  let yDiff = fromIntegral (yEnd - yStart) :: Float
  let xDiff = fromIntegral (xEnd - xStart) :: Float
  let slope = if xDiff > yDiff then yDiff / xDiff else xDiff / yDiff

  if isNaN slope
    then return [(xStart, yStart)]
    else
      if abs (x1 - x2) > abs (y1 - y2)
        then return [(x, round $ fromIntegral yStart + (fromIntegral (x - min xStart xEnd) * slope)) | x <- [min xStart xEnd .. max xStart xEnd]]
        else return [(round $ fromIntegral xStart + (fromIntegral (y - min yStart yEnd) * slope), y) | y <- [min yStart yEnd .. max yStart yEnd]]

pointToCoord :: Float -> Int -> Int
pointToCoord pointValue size = round $ abs ((-1 - pointValue) / (2 / fromIntegral size))

isLiquid :: ParticleType -> Bool
isLiquid p
  | p == Water || p == Acid = True
  | otherwise = False