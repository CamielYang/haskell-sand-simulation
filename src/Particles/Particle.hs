module Particles.Particle
  ( module Particles.Particle,
    module Particles.Liquid.Liquids,
    module Particles.MovableSolid.MovableSolids,
  )
where

import Graphics.Gloss.Raster.Array
import Particles.Liquid.Liquids
import Particles.MovableSolid.MovableSolids
import Types

allParticles :: [ParticleType]
allParticles = [(minBound :: ParticleType) ..]

defaultParticle =
  Particle
    { pFallRate = 1,
      pColor = white
    }

defaultStaticParticle =
  Particle
    { pFallRate = 0,
      pColor = white
    }

pd :: ParticleType -> ParticleData
pd Sand = defaultParticle {pColor = rgbI 218 211 165}
pd Water = defaultParticle {pColor = rgbI 5 138 189}
pd Acid = defaultParticle {pColor = rgbI 114 209 68}
pd Stone = defaultStaticParticle {pColor = rgbI 185 185 185}
pd Wood = defaultStaticParticle {pColor = rgbI 136 118 71}
pd Grass = defaultStaticParticle {pColor = rgbI 101 159 72}
pd _ = defaultStaticParticle {pColor = white}