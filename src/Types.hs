module Types where

import Data.Array (Array)
import Data.Array.IO (IOArray)
import Graphics.Gloss (Color)

data ParticleType
  = Sand
  | Water
  | Stone
  | Wood
  | Grass
  | Acid
  | Empty
  deriving (Eq, Show, Enum, Bounded)

data ParticleData = Particle
  { pFallRate :: Int,
    pColor :: Color
  }

type Coord = (Int, Int)

type Cell = (ParticleType, Bool)

type GridA a = a Coord Cell

type Grid = GridA IOArray

type ImmutableGrid = GridA Array

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
    immutableGrid :: ImmutableGrid,
    toggleUpdated :: Bool,
    mouseDown :: Bool,
    mousePosition :: Coord,
    prevMousePosition :: Coord,
    selectedParticle :: ParticleType
  }