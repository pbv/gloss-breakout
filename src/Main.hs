{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | the complete game state
data GameState
  = GameState { paddleX :: Float -- ^ x-coord of paddle
              , ballPos :: Point  -- ^ current ball position
              , prevPos :: Point  -- ^ ball position on previous frame
              , ballVel :: Vector -- ^ current ball velocity
              , blocks :: [Block] -- ^ list of blocks
              , score :: Int      -- ^ current player score
              , faults :: Int     -- ^ number of balls dropped
              , paused :: Bool    -- ^ is the game paused?
              } deriving Show

data Block
  = Block { blkCoord :: Grid
          , blkColor :: Color
          , blkScore :: Int
          } deriving Show

-- | discrete x,y block coordinates 
type Grid = (Int,Int)

-- | conversion between screen and grid coordinates
toGrid :: Point -> Grid
toGrid (x,y) = (col,row)
  where
    row = round (y / blockHeight)
    col = round (x / blockWidth)

toScreen :: Grid -> Point
toScreen (col,row) = (x,y)
  where
    x = fromIntegral col * blockWidth
    y = fromIntegral row * blockHeight


-- | initial state
initial :: GameState
initial = GameState { paddleX = 0
                    , ballPos = (0,paddleY+paddleHeight)
                    , prevPos = ballPos initial
                    , ballVel = (200,200)
                    , blocks = initialBlocks
                    , score = 0
                    , faults = 0
                    , paused = True                    
                    }

-- | list of blocks at the start of the game
initialBlocks :: [Block]
initialBlocks
  = [ Block (col,row) color pts
    | (row, color, pts) <-
        zip3 [0..4] [blue, green, yellow, orange, red] [10, 20, 50, 100]
    , col <- [-10..10] 
    ]
    


-- | render a complete game 
render :: GameState -> Picture
render GameState{..} 
  = pictures (drawPaddle paddleX :
              drawBall ballPos :
              drawScore faults score :
              drawFrame :
              map drawBlock blocks)

drawBlock :: Block -> Picture
drawBlock (Block pt c _)
   = translate x y $ color c $ rectangleSolid (blockWidth-2) (blockHeight-2)
  where (x,y) = toScreen pt

drawPaddle :: Float -> Picture
drawPaddle x
  = translate x paddleY $
    color white $ rectangleSolid paddleWidth paddleHeight

drawScore :: Int -> Int -> Picture
drawScore faults score = pictures [p1, p2]
  where p1 = translate 400 200 $
             color white $
             scale 0.25 0.25 $ text (show score)
        p2 = translate (-400) 200 $
             color yellow $
             scale 0.25 0.25 $ text (show faults)

-- | draw frame around the game play area          
drawFrame :: Picture
drawFrame
  = color white $
    rectangleWire (fromIntegral width - 2 ) (fromIntegral height - 2)

-- | draw a single ball
drawBall :: Point -> Picture
drawBall (x,y)
  = translate x y $ color white $ circleSolid ballRadius

-- | constants
-- block dimensions
blockWidth, blockHeight :: Float
blockWidth = 48
blockHeight = 32

-- | paddle dimensions
paddleWidth = 192
paddleHeight = 16

paddleY = -maxY + 2*paddleHeight

-- | ball dimensions
ballRadius :: Float
ballRadius = 8


-- | geometric utilities
-- spacial relations between coordinates
above, below, left, right :: Grid -> Grid -> Bool
(x,y) `above` (x',y') = y > y'
(x,y) `below` (x',y') = y < y'
(x,y) `left` (x',y') = x < x'
(x,y) `right` (x',y') = x > x'


-- | check ball colision with any blocks
collision :: GameState -> GameState
collision game@GameState{..}
  | paused    = game
  | otherwise = case hits of
                  (_, []) -> game
                  (before, (Block _ _ pts) : after) ->
                    game { score = score + pts
                         , ballVel = reflect coord' coord ballVel
                         , blocks = before ++ after
                         }
  where coord = toGrid ballPos
        coord'= toGrid prevPos
        hits =  break (\blk -> blkCoord blk == coord) blocks

-- | change ball velocity acording to the hit direction
reflect :: Grid -> Grid -> Vector -> Vector
reflect old new (vx,vy) = (reflectX, reflectY)
  where 
    reflectX = if new `left` old || new `right` old then -vx else vx
    reflectY = if  new `above` old || new `below` old then -vy else vy
    

-- | check colision with the paddle
deflect :: GameState -> GameState
deflect game@GameState{..}
  | paused                   = game
  | y > paddleY+paddleHeight = game
  | paddleX-paddleWidth/2 <= x && x <= paddleX+paddleWidth/2
       = game { ballVel = (vx, -vy) }
  | otherwise
       =  game { ballPos = (paddleX,paddleY+paddleHeight)
               , prevPos = (paddleX,paddleY+paddleHeight)
               , ballVel = (200,200)
               , faults  =  1 + faults
               , paused = True
               }
  where (x,y) = ballPos
        (vx,vy) = ballVel
        

-- | update ball motion
motion :: Float -> GameState -> GameState
motion dt game@GameState{..}
  | paused = game
  | otherwise = game { ballPos = ballPos'
                     , prevPos = ballPos
                     , ballVel = balVel'
                     }
  where (ballPos', balVel') = bounce (advance dt ballVel ballPos) ballVel
  
-- | straight linear motion
advance :: Float -> Vector -> Point -> Point
advance dt (dx,dy) (x,y) = (x',y')
  where x' = x + dx*dt
        y' = y + dy*dt

-- | bounce on walls
bounce :: Point -> Vector -> (Point, Vector)
bounce (x,y) (dx,dy) = ((x',y'), (dx',dy'))
  where (x',dx') = clip x dx (maxX - ballRadius)
        (y',dy') = clip y dy (maxY - ballRadius)
        clip h dh max
          | h > max  = (max, -dh)
          | h < -max = (-max, -dh)
          | otherwise = (h, dh)

-- | event handling
react :: Event -> GameState -> GameState
react (EventMotion (x,y)) game@GameState{..}
  | paused    = game { paddleX = paddleX'
                     , ballPos = (paddleX, paddleY+paddleHeight)
                     }
  | otherwise = game { paddleX = paddleX' }
    where
    paddleX' = (maxX - paddleWidth/2) `min` x `max` (-maxX + paddleWidth/2)


react (EventKey (MouseButton LeftButton) Down _ _) game
      = game { paused=False }
react _ game
      = game


-- | application start point
main :: IO ()
main = 
  play window black fps initial render react (\dt -> deflect . collision . motion dt)

fps :: Int
fps = 60

window :: Display
window = FullScreen  (width,height) 

maxX, maxY :: Float
maxX = fromIntegral width/2
maxY = fromIntegral height/2

width, height :: Int
width = 1024
height = 768
