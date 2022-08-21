module Config where

import Graphics.Gloss
import Types



-- Size of the window
gameSize :: (Int, Int)
gameSize = (800,800)

-- Position of the window on the screen
gamePosition :: (Int, Int)
gamePosition = ((1920-gameWidth) `div` 2, (1080-gameHeight) `div` 2)

-- Getters for the game width and height
gameWidth  = fst gameSize
gameHeight = snd gameSize


-- Information on the display
windowDisplay :: Display
windowDisplay = InWindow "Window" gameSize gamePosition

-- Rate at which the game is updated
simulationRate :: Int
simulationRate = 24







-- PLAYER STUFF

defaultPlayerSpeed :: Float
defaultPlayerSpeed = 3

defaultPlayerSize :: Size
defaultPlayerSize = 10


-- Enemy Stuff

defaultEnemySpeed :: Float 
defaultEnemySpeed = 10

defaultEnemySize :: Size
defaultEnemySize = 15


-- Bullet Stuff

defaultBulletSpeed :: Float 
defaultBulletSpeed = 5

defaultBulletSize :: Size 
defaultBulletSize = 5