{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Control.Parallel.Strategies
import Data.List (zipWith6)
import Control.DeepSeq (NFData, rdeepseq)
import GHC.Generics (Generic)

data Particle = Particle
  { px :: Float
  , py :: Float
  , pr :: Float
  , pa :: Float
  , pspeed :: Float  -- renamed from 'ps' to avoid name clash
  , pc :: Color
  }

width, height :: Int
width = 1200
height = 800
bhRadius :: Float
bhRadius = 50
numDiskParticles :: Int
numDiskParticles = 15000
numStars :: Int
numStars = 5000
warpK :: Float
warpK = 15000

genDisk :: Int -> [Particle]
genDisk n =
  let rng = mkStdGen 42
      radii = [bhRadius + fromIntegral i * 0.03 | i <- [1..n]]
      angles = take n $ randomRs (0, 2*pi) rng
      speeds = map (\r -> 80 / r) radii
      spiralOffset = map (\r -> r*0.15) radii
      colors = map (\r -> makeColor (min 1 (1/r*10+0.4)) 0.2 (max 0 (1 - r/400)) 1) radii
  in zipWith6 Particle (repeat 0) (repeat 0) radii (zipWith (+) angles spiralOffset) speeds colors

genStars :: Int -> [Particle]
genStars n =
  let rng = mkStdGen 99
      radii = take n $ randomRs (bhRadius+150,2000) rng
      angles = take n $ randomRs (0, 2*pi) rng
      colors = replicate n white
      speeds = replicate n 0
  in zipWith6 Particle (repeat 0) (repeat 0) radii angles speeds colors

updateParticles :: Float -> [Particle] -> [Particle]
updateParticles dt ps =
  withStrategy (parListChunk 500 rdeepseq) $
    map (\p -> 
      let spd = pspeed p
          r = pr p
          warpedAngle = pa p + spd*dt + warpK / (r*r + 20000)
          newX = r * cos warpedAngle
          newY = r * sin warpedAngle
      in p { pa = warpedAngle, px = newX, py = newY }
    ) ps

particleToPicture :: Particle -> Picture
particleToPicture Particle{..} =
  translate px py $ color pc $ circleSolid (if pr < 400 then 2 else 1)

drawBH :: Picture
drawBH = color black $ circleSolid bhRadius

render :: ([Particle], [Particle]) -> Picture
render (stars,disk) =
  pictures $ drawBH : map particleToPicture stars ++ map particleToPicture disk

-- Dummy event handler (ignore input)
handleEvent :: Event -> ([Particle], [Particle]) -> ([Particle], [Particle])
handleEvent _ w = w

step :: Float -> ([Particle], [Particle]) -> ([Particle], [Particle])
step dt (stars, disk) = (stars, updateParticles dt disk)

main :: IO ()
main = do
  let disk = genDisk numDiskParticles
      stars = genStars numStars
  play
    (InWindow "Gargantua Black Hole Simulation" (width,height) (50,50))
    black
    30                 -- FPS
    (stars,disk)       -- initial world
    render             -- render function
    handleEvent        -- event handler
    step               -- step function

