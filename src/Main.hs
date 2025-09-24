{-# LANGUAGE RecordWildCards #-}

import Graphics.UI.GLUT hiding (Disk)
import System.Random
import Data.IORef
import Control.Monad (forM, forM_)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)

-- Differentiates between the various components of our simulation
data ParticleType = Disk | Halo | Star
  deriving (Eq, Show)

-- Particle structure
data Particle = Particle
  { pPos         :: !(Vertex3 GLfloat)
  , pColor       :: !(Color3 GLfloat)
  , pRadius      :: !GLfloat
  , pAngle       :: !GLfloat
  , pOrbitalSpeed:: !GLfloat
  , pInwardSpeed :: !GLfloat
  , pPlaneOffset :: !GLfloat
  , pType        :: !ParticleType
  }

-- A record to hold the state for our FPS counter
data FPSState = FPSState
  { frameCount :: !Int
  , startTime  :: !Int
  , currentFPS :: !Float
  }

-- Simulation Constants
numDiskParticles :: Int
numDiskParticles = 100000

numHaloParticles :: Int
numHaloParticles = 45000

numStars :: Int
numStars = 500

diskRadiusMin, diskRadiusMax :: GLfloat
diskRadiusMin = 3.5
diskRadiusMax = 16.0

-- Helper to interpolate between two colors.
lerpColor :: GLfloat -> Color3 GLfloat -> Color3 GLfloat -> Color3 GLfloat
lerpColor t (Color3 r1 g1 b1) (Color3 r2 g2 b2) =
  let lerp v1 v2 = v1 * (1-t) + v2 * t
  in Color3 (lerp r1 r2) (lerp g1 g2) (lerp b1 b2)

-- Multi-stage color gradient
getColorForDistance :: GLfloat -> Color3 GLfloat
getColorForDistance r =
  let hotColor  = Color3 1.0 1.0 0.95
      midColor  = Color3 1.0 0.8 0.2
      coolColor = Color3 1.0 0.5 0.1
      t = (r - diskRadiusMin) / (diskRadiusMax - diskRadiusMin)
  in if t < 0.3
     then lerpColor (t / 0.3) hotColor midColor
     else lerpColor ((t - 0.3) / 0.7) midColor coolColor

-- Generic particle generator
genParticles :: Int -> ParticleType -> IO [Particle]
genParticles count pType = forM [1..count] $ \_ -> do
    t <- (**1.5) <$> randomRIO (0.0, 1.0) :: IO GLfloat
    let r = case pType of
          Disk -> diskRadiusMin + t * (diskRadiusMax - diskRadiusMin)
          Halo -> diskRadiusMin + t * (diskRadiusMax * 0.9 - diskRadiusMin)
          Star -> 50 + t * 50

    angle <- randomRIO (0, 2*pi)
    (pos, orbitalSpeed, inwardSpeed, planeOffset) <- case pType of
        Disk -> do
            z <- randomRIO (-0.08, 0.08)
            os <- randomRIO (0.02, 0.05)
            is <- randomRIO (0.001, 0.004)
            return (Vertex3 (r * cos angle) (r * sin angle) z, os, is, z)
        Halo -> do
            y <- randomRIO (-0.4, 0.4)
            os <- randomRIO (0.02, 0.05)
            is <- randomRIO (0.001, 0.004)
            return (Vertex3 (r * cos angle) y (r * sin angle), os, is, y)
        Star -> do
            angle2 <- randomRIO (0, 2*pi)
            let (x,y,z) = (r * cos angle * sin angle2, r * sin angle * sin angle2, r * cos angle2)
            return (Vertex3 x y z, 0, 0, 0)
    let color = case pType of
            Star -> let c = 0.5 in Color3 c c c
            _    -> getColorForDistance r
    return $ Particle pos color r angle orbitalSpeed inwardSpeed planeOffset pType


-- Update and recycle particles
updateParticle :: Particle -> IO Particle
updateParticle p | pType p == Star = return p
updateParticle p@Particle{..} = do
    let newAngle = pAngle + pOrbitalSpeed / sqrt pRadius / 20.0
        inwardDrift = pInwardSpeed / (pRadius * pRadius + 0.1)
        newRadius = pRadius - inwardDrift
    if newRadius < diskRadiusMin
    then do
        recycledAngle <- randomRIO (0, 2 * pi)
        let recycledRadius = diskRadiusMax
            recycledColor = getColorForDistance recycledRadius
            recycledPos = case pType of
                Disk -> Vertex3 (recycledRadius * cos recycledAngle) (recycledRadius * sin recycledAngle) pPlaneOffset
                Halo -> Vertex3 (recycledRadius * cos recycledAngle) pPlaneOffset (recycledRadius * sin recycledAngle)
                _    -> pPos
        return $ p { pRadius = recycledRadius, pAngle = recycledAngle, pPos = recycledPos, pColor = recycledColor }
    else do
        let newPos = case pType of
                Disk -> Vertex3 (newRadius * cos newAngle) (newRadius * sin newAngle) pPlaneOffset
                Halo -> Vertex3 (newRadius * cos newAngle) pPlaneOffset (newRadius * sin newAngle)
                _    -> pPos
        return $ p { pRadius = newRadius, pAngle = newAngle, pPos = newPos }

-- Render particles
renderParticle :: Particle -> IO ()
renderParticle p@Particle{..} =
  let Particle { pPos = Vertex3 _ py _, .. } = p
      (brightness, dopplerColor) = case pType of
        Disk ->
          let sideFactor = -py / pRadius
              brightnessMod = 1.0 + sideFactor * 1.0
              blueShift = max 0 $ sideFactor * 0.8
          in (brightnessMod, Color3 ((\(Color3 r _ _) -> r) pColor) ((\(Color3 _ g _) -> g) pColor + blueShift*0.5) ((\(Color3 _ _ b) -> b) pColor + blueShift))
        _ -> (1.0, pColor)
  in preservingMatrix $ do
      color $ dopplerColor `multC` brightness
      let Vertex3 x y z = pPos in translate (Vector3 x y z)
      renderPrimitive Points $ vertex (Vertex3 0 0 0 :: Vertex3 GLfloat)
  where multC (Color3 r g b) s = Color3 (r*s) (g*s) (b*s)


-- THE FIX IS HERE: This function now correctly isolates its state changes.
renderString2D :: String -> IO ()
renderString2D str = do
    (Size w h) <- get windowSize
    -- Switch to Projection matrix stack and save its state
    matrixMode $= Projection
    preservingMatrix $ do
        -- Set up a new 2D orthographic projection
        loadIdentity
        ortho2D 0 (realToFrac w) 0 (realToFrac h)

        -- Switch to Modelview matrix stack and save its state
        matrixMode $= Modelview 0
        preservingMatrix $ do
            -- Set up a new identity modelview for 2D drawing
            loadIdentity

            -- Render the text in the top-left corner
            depthFunc $= Nothing -- Disable depth testing for the overlay
            color (Color3 1 1 1 :: Color3 GLfloat)
            rasterPos (Vertex2 10 (realToFrac h - 20) :: Vertex2 GLfloat)
            renderString Fixed8By13 str
            depthFunc $= Just Less -- Re-enable depth testing

    -- Crucially, switch back to Modelview mode for the next frame's 3D rendering
    matrixMode $= Modelview 0

-- Display callback
display :: IORef [Particle] -> IORef GLfloat -> IORef FPSState -> DisplayCallback
display particlesRef camAngleRef fpsRef = do
    -- FPS Calculation Logic
    currentTime <- get elapsedTime
    fpsState <- get fpsRef
    let timeDiff = currentTime - startTime fpsState
    
    if timeDiff > 500
      then do
        let newFPS = fromIntegral (frameCount fpsState) * 1000 / fromIntegral timeDiff
        atomicWriteIORef fpsRef $ FPSState 0 currentTime newFPS
      else
        modifyIORef' fpsRef $ \s -> s { frameCount = frameCount s + 1 }

    -- 3D Scene Rendering
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    
    camAngleFloat <- get camAngleRef
    let camAngle = realToFrac camAngleFloat :: Double
    let camRadius = 30.0
        camPos = Vertex3 (camRadius * sin camAngle) 2.5 (camRadius * cos camAngle)
    lookAt camPos (Vertex3 0 0 0) (Vector3 0 1 0)

    color (Color3 0 0 0 :: Color3 GLfloat)
    renderObject Solid (Sphere' (realToFrac diskRadiusMin - 0.2) 32 32)
    
    preservingMatrix $ do
      color (Color3 1 1 1 :: Color3 GLfloat)
      let ringRadius = diskRadiusMin - 0.15; segments = 100
          points = [ Vertex3 (ringRadius * cos a) (ringRadius * sin a) 0 | i <- [0..segments], let a = fromIntegral i / fromIntegral segments * 2 * pi ]
      depthFunc $= Nothing
      renderPrimitive LineLoop $ mapM_ vertex points
      depthFunc $= Just Less

    particles <- get particlesRef
    forM_ particles renderParticle

    -- 2D Overlay Rendering
    fps <- currentFPS <$> get fpsRef
    renderString2D (printf "FPS: %.1f" fps)
    
    swapBuffers

-- Idle callback
idle :: IORef [Particle] -> IORef GLfloat -> IdleCallback
idle particlesRef camAngleRef = do
    threadDelay (1000000 `div` 60)
    currentParticles <- get particlesRef
    newParticles <- mapM updateParticle currentParticles
    atomicWriteIORef particlesRef newParticles
    modifyIORef' camAngleRef (+0.0025)
    postRedisplay Nothing

-- Reshape callback
reshape :: ReshapeCallback
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (fromIntegral w / fromIntegral h) 1.0 200.0
    matrixMode $= Modelview 0

-- Main function
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    _window <- createWindow "Gargantua Simulation v4.1 - Corrected"
    windowSize $= Size 1200 900
    clearColor $= Color4 0 0 0 1
    depthFunc $= Just Less
    blend $= Enabled
    blendFunc $= (SrcAlpha, One)
    pointSize $= 1.5
    pointSmooth $= Enabled

    allParticles <- (++) <$> genParticles numDiskParticles Disk
                         <*> ((++) <$> genParticles numHaloParticles Halo
                                   <*> genParticles numStars Star)

    particlesRef <- newIORef allParticles
    camAngleRef <- newIORef 0.0

    startTime <- get elapsedTime
    let initialFPSState = FPSState 0 startTime 0.0
    fpsRef <- newIORef initialFPSState
    
    displayCallback $= display particlesRef camAngleRef fpsRef
    idleCallback $= Just (idle particlesRef camAngleRef)
    reshapeCallback $= Just reshape
    mainLoop
