module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (StdGen, mkStdGen, getStdGen, randomR)
import Control.Monad.State (State, get, put, runState, replicateM, state)

import Data.Map (Map)
import qualified Data.Map as Map

import Config
import Types



-----------------------------------------------------------------------------------------------
-- Functions for generating new entities



-- Generate random locations
generateRandomLocation :: (Float, Float) -> Location -> State StdGen Location
generateRandomLocation (distanceX, distanceY) (playerX, playerY) = do
    -- Get generator (will usually be the world random generator)
    gen <- get

    -- Calculate start position
    let 
        -- Generate random number between 200 and the distance passed in
            -- This is because enemies and packages must spawn away from the player so there is no potential for it to instantly intersect
            -- Also create new generator to use for angle
        (distance, gen') = randomR (200, distanceX) gen 
        -- Generate a random angle from the distance, think polar coordinates which are later converted to cartesian
            -- Use generator generated from last computation
        (angle, gen'') = randomR (0, pi * 2) gen'

        -- Convert distance and angle to cartesian coordinates
        startX = cos angle * distance
        startY = sin angle * distance

        -- Locations are provided relative to the player, and if the location would be out of the bounds of the window size,
            -- Change the spawn location so it spawns within the bounds of the window size
            -- Do this by negating the startX or startY value
        startX' = if (abs (playerX + startX) > 400) then (playerX - startX) else (playerX + startX)
        startY' = if (abs (playerY + startY) > 400) then (playerY - startY) else (playerY + startY)

    -- Returns the location of the entity and the new generator
    put gen''
    return (startX' , startY')


-- Generates random coefficients for entities - used for size, speed and health
generateRandomCoefficients :: (Float, Float) -> State StdGen Float
generateRandomCoefficients (smallest, largest) = do
    -- Get generator (will usually be the world random generator)
    gen <- get

    -- Generate random float value between the two floats passed in
    let (coeficient, gen') = randomR (smallest, largest) gen
    -- Returns the generator and the random float generated
    put gen'
    return (coeficient)





-- Generating new enemies in the world
generateNewEnemies :: World -> World
-- Combines the list of new enemies with the list of the old entities in the world and updates the randomGenerator
generateNewEnemies w = w {entities = (entities w) ++ newEntities, worldRandomGenerator = gen'}
    where 
        -- Number of enemies to be generated is the next level number squared 
        numberOfEnemies = (level (worldStats w) + 1) 

        -- Get the generator and the random location out of the state monad and replicate the random directions
        (randomLocations, gen) = runState
            -- Use 400 400 because that is the distance from the center to the edge of the screen
            (replicateM numberOfEnemies (generateRandomLocation (400,400) (playerLocation (player w))))
            (worldRandomGenerator w)
        
        -- Get the generator and the random coefficient out of the state monad
        (randomCoefficients, gen') = runState
            -- Coefficient is between 1 and 5 (try to use 1 as the smallest or largest value as it preserves some of the defaults when generating sizes and health)
            (replicateM numberOfEnemies (generateRandomCoefficients (1, 5) ))
            gen

        -- Generate generic enemies 
        blankEntities = replicate numberOfEnemies (Entity {entityType = Enemy, entityHealth = 100, entitySize = defaultEnemySize, entityLocation = (0,0), entityInfo = EnemyInfo {enemySpeed = defaultEnemySpeed} })

        -- Create new entities with random properties
        newEntities = createNewEnemies blankEntities randomLocations randomCoefficients

        -- Function to create new entities with random properties
            -- Pass in an entity list (must be all enemies), a list of locations and a list of floats (coefficients for speed size and health)
        createNewEnemies :: [Entity] -> [Location] -> [Float] -> [Entity]
        -- Assume all lists are of the same length
        createNewEnemies [] [] [] = []
        -- Iterate through these lists 
        createNewEnemies (e : es) (l : ls) (c : cc) = e -- Modify each enemy in the list to have these new properties
                                                { entityLocation = l
                                                , entitySize = defaultEnemySize / c
                                                , entityHealth = 500 / (c ** 4)
                                                , entityInfo = (entityInfo e) {enemySpeed = c} }
                                                 : createNewEnemies es ls cc -- recursively go through the list of entities so that they are all modified

-- Generate a supply package in a random location
generateSupplyPackage :: World -> World
-- There should only be one supply package generated, so can use cons to combine this with the rest of the entities
generateSupplyPackage w = w {entities = newEntity : (entities w), worldRandomGenerator = gen''}
    where 
        -- Generate a single random location based on the center of the screen
        (randomLocation, gen'') = runState
            (generateRandomLocation (400,400) ((0,0)))
            (worldRandomGenerator w)
        -- Create new supply package entity with following properties
        newEntity = Entity {entityType = SupplyPackage, entityHealth = 1, entitySize = 5, entityLocation = randomLocation, entityInfo = SupplyPackageInfo }


-- Create new bullet
    -- Take list of entities, location of player and location of mouse click to return new list of entities
newBullet :: [Entity] -> Location -> Location -> [Entity]
-- Combine new bullet with existing list of entities
newBullet e (x,y) (clickx, clicky) = bullet : e
    where 
        -- Create new entity of type bullet and following parameters
        bullet = Entity {
            entityType = Bullet
          , entityHealth = 1
          , entitySize = defaultBulletSize
          , entityLocation = (x,y)
          , entityInfo = (BulletInfo {startLocation = (x,y), direction = angle, bulletSpeed = defaultBulletSpeed})
        }
        
        -- Calculate the angle the bullet should travel
        -- Angle calculated by arctan function will be between -pi and pi, so may require addition of pi
        angle = atan m + addPi
        m = (clicky - y) / (clickx - x)
        -- Function to add pi if necessary
            -- Consider x,y to be the origin
            -- If the click is to the left of the origin, should add pi else dont add anything
        addPi 
            | clickx < x = pi
            | otherwise = 0

-----------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------
-- Functions for calculating positions


-- Calculate the distance between two locations 
distance :: Location -> Location -> Float
distance (x1,y1) (x2,y2) = sqrt ( (abs (x2-x1))**2 + abs (y2-y1)**2 )


-----------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------
-- Intersect methods


-- Find if one entity intersects with another
    -- Return true for there is an intersection and false for there is no intersection
intersect :: Location -> Size -> Location -> Size -> Bool
-- Take entity 1 size and location and entity 2 size and location to return a boolean
intersect (x1,y1) size1 (x2,y2) size2
    -- If the difference between the entities is less than the distance from the center of one entity to another
    | abs (x1 - x2) < minSpaceBetween && abs (y1 - y2) < minSpaceBetween = True
    | otherwise = False
        -- Not divided by 2 as size is the size of the entity from the center to the edge of an entity
        where minSpaceBetween = size1 + size2


-- Find if one entity intersects with a group of otherentities
intersectEntities :: Location -> Size -> [Entity] -> Bool
-- Base case means that there are no intersections
intersectEntities _ _ [] = False
-- Recursively look through entities to see if any intersect
intersectEntities (x,y) size (e : es)
    -- If there is an intersection, return true
    -- Call the previously defined intesect method
    | intersect (x,y) size (entityLocation e) (entitySize e) = True
    -- If they don't intersect, look at the next entity to see if it does
    | otherwise = intersectEntities (x,y) size es

-----------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------
-- Getter functions for entities


-- Get all of the bullets from a list of entities
    -- Takes a list of entities and returns a list of entities (all of them will have type Bullet)
bullets :: [Entity] -> [Entity] 
bullets [] = []
bullets (e : es) 
    -- If the type is bullet, add it to the list 
    | entityType e == Bullet = e : bullets es
    -- If the type is not bullet, do not add to the list
    | otherwise = bullets es


-- Get all of the enemies from a list of entities
    -- Takes a list of entities and returns a list of entities (all of them will have type Enemies)
enemies :: [Entity] -> [Entity] 
enemies [] = []
enemies (e : es) 
    | entityType e == Enemy = e : enemies es
    | otherwise = enemies es


-- Get all of the supply packages from a list of entities
    -- Takes a list of entities and returns a list of entities (all of them will have type SupplyPackage)
supplyPackages :: [Entity] -> [Entity] 
supplyPackages [] = []
supplyPackages (e : es) 
    | entityType e == SupplyPackage = e : supplyPackages es
    | otherwise = supplyPackages es

-----------------------------------------------------------------------------------------------



-- Resets the world so player can play again
    -- Takes the current world variable and changes all of the elements that need to be changed
restartWorld :: World -> World
restartWorld w = w { player = Player  -- Reset player to have full health and start at the center
                { playerLocation = (0,0)
                , playerHealth = 100
                , playerSize = defaultPlayerSize
                }
            , entities = [] -- Reset entities so that there are no entities to begin
            , worldStats = Stats {level = 0} -- Reset level counter
            , gameState = Title -- Show the title screen
            }


-- Main function to run
main :: IO ()
main = do

    -- Make a standard generator to use for the world random generator
    gen <- getStdGen
    let 

        -- Create a player record
        player = Player 
            { playerLocation = (0,0)
            , playerHealth = 100
            , playerSize = defaultPlayerSize
            }

        -- Create a stats record
        stats = Stats {level = 0}

        -- Create a record of the keys necessary (only arrow keys)
        keyBoardMap :: Map String Bool
        keyBoardMap = Map.fromList [
              ("KeyUp", False)
            , ("KeyDown", False)
            , ("KeyRight", False)
            , ("KeyLeft", False)
            ]

        -- Create the initial world state
        initialWorld = World 
            { player = player
            , gameState = Title
            , worldRandomGenerator = gen
            , entities = []
            , worldStats = stats
            , keyBoardInput = keyBoardMap
            }

    -- Gloss play function
    play
        windowDisplay -- Found in config
        white
        simulationRate -- Found in config
        initialWorld -- Found below
        drawingFunc -- Found below
        inputHandler -- Found below
        updateFunc -- Found below









-- Input handler functions 
inputHandler :: Event -> World -> World
-- First set of input handler cases are for the arrow keys
    -- If a key has an event, the boolean value in the keyBoardInput map should be changed e.g. if true make it false
    -- This works as the first time the key up is pressed, the value is set to true. The next event will be a keyup, so change the value back to false
    
    -- Sidenote - the movement is not handled here because the user may want to move in two directions at the same time
        -- E.g. if the player presses the up key and the right key, the player will only move up as the up pattern comes sequentially first
inputHandler (EventKey (SpecialKey KeyUp) _ _ _) w = w {keyBoardInput = Map.adjust (changeBool) "KeyUp" (keyBoardInput w)}
inputHandler (EventKey (SpecialKey KeyDown) _ _ _) w = w {keyBoardInput = Map.adjust (changeBool) "KeyDown" (keyBoardInput w)}
inputHandler (EventKey (SpecialKey KeyRight) _ _ _) w = w {keyBoardInput = Map.adjust (changeBool) "KeyRight" (keyBoardInput w)}
inputHandler (EventKey (SpecialKey KeyLeft) _ _ _) w = w {keyBoardInput = Map.adjust (changeBool) "KeyLeft" (keyBoardInput w)}


-- The next key events are for events that can be handled here 

-- If p is pressed, change state of game from paused to playing and vice versa
inputHandler (EventKey (Char 'p') Down _ _) w = w {gameState = pauseButton (gameState w)}

-- If r is pressed, restart the world with restartWorld function 
inputHandler (EventKey (Char 'r') Down _ _) w = restartWorld w

-- If if the left mouse button is clicked, create a new bullet 
inputHandler (EventKey (MouseButton LeftButton) Down _ position) w = w {entities = (newBullet (entities w) (playerLocation (player w)) position)}
       
-- Otherwise world remains the same      
inputHandler _ w = w


-- Function to flip the value of a boolean e.g. if true make it false
changeBool :: Bool -> Bool
changeBool False = True
changeBool True = False

-- Function to change if game is paused or not
pauseButton :: GameState -> GameState
pauseButton s
    | s == Playing = Paused
    | otherwise = Playing










-- Update function
updateFunc :: Float -> World -> World
updateFunc _ w 
    -- If the game state is not playing, do not update the world
    | gameState w /= Playing = w
    -- If the game state is playing, can update the world as follows
    | otherwise = w {
        -- Tick all of the relevant fields in the world
          player = tickPlayer (player w)
        , worldStats = tickWorldStats (worldStats w)
        , entities = checkEntities (entities w)
        , worldRandomGenerator = tickWorldRandomGenerator (worldRandomGenerator w)
        , gameState = tickGameState (gameState w)
        }
        where



    ---------------------------------------------------------------
    -- GAME STATE 

    -- Check if player has died
    tickGameState :: GameState -> GameState
    tickGameState gs
        -- If their health is less than or equal to 0, the game should end
        | playerHealth (player w) <= 0 = GameOver
        | otherwise = gs

    ---------------------------------------------------------------

    ---------------------------------------------------------------
    -- STATS

    -- Check to see if there are no enemies - if there are none, player has defeated a level so levelup
    tickWorldStats :: Stats -> Stats
    tickWorldStats s
        -- If the list of enemies is null, increment the level number
        | null $ enemies $ entities w = s {level = level s + 1}
        | otherwise = s 

    ---------------------------------------------------------------



    ---------------------------------------------------------------
    -- RANDOM GENERATOR

    -- Set the world random generator to depend on the level the user is currently on
    tickWorldRandomGenerator :: StdGen -> StdGen
    tickWorldRandomGenerator g = mkStdGen (level $ worldStats w)

    ---------------------------------------------------------------



    ---------------------------------------------------------------
    -- PLAYER
    
    -- Tick fields in player
    tickPlayer :: Player -> Player
    tickPlayer p = p { 
        playerLocation = tickPlayerLocation (playerLocation p) 
      , playerHealth = tickPlayerHealth (playerHealth p)
    }

    
    -- Update the player health
    tickPlayerHealth :: Health -> Health
    tickPlayerHealth h 
        | h > 100 = 100 -- The max player health is 100, so if health is greater set back to 100
            -- If the player intersects with an enemy, reduce health by 5
        | intersectEntities location size (enemies (entities w)) = h - 5 
            -- If the player intersects with a supply package, increase health by 10
        | intersectEntities location size (supplyPackages (entities w)) = h + 10 
            -- Otherwise keep health the same
        | otherwise = h
            where location = (playerLocation (player w))
                  size = (playerSize (player w))


    -- Updates players location
    tickPlayerLocation :: Location -> Location
    -- Make new location equal to the original location plus the difference in the x value and the y value
    tickPlayerLocation (x,y) = (x + diffX, y + diffY)
        where 
            -- If the kep is being pressed, set value to player speed as that is how much it should move in that direction
            up = if (lookup "KeyUp" (Map.toList (keyBoardInput w)) == Just True) then defaultPlayerSpeed else 0
            down = if (lookup "KeyDown" (Map.toList (keyBoardInput w)) == Just True) then defaultPlayerSpeed else 0
            right = if (lookup "KeyRight" (Map.toList (keyBoardInput w)) == Just True) then defaultPlayerSpeed else 0
            left = if (lookup "KeyLeft" (Map.toList (keyBoardInput w)) == Just True) then defaultPlayerSpeed else 0

            -- The difference is calculated as so:
                -- If only right is being pressed, left will be 0 and right will be the defaultPlayerSpeed
            diffX = right - left
            diffY = up - down

    ---------------------------------------------------------------


    ---------------------------------------------------------------
    -- Entities


    -- Check entities is called before tick entities as a check to see if there enemies must first be done
    checkEntities :: [Entity] -> [Entity]
    checkEntities e 
        -- If there are no enemies and the level is a multiple of 5, generate new enemies and a new supply package
        | (null $ enemies e) && level (worldStats w) `mod` 5 == 0 = entities (generateNewEnemies w) ++ entities (generateSupplyPackage w)
        -- If there are only new enemies (and the level is not a multiple of 5), generate a new supply package 
        | null $ enemies e = entities (generateNewEnemies w)
        -- If there are enemies, then all entities should be ticked
        | otherwise = tickEntities e

    -- Tick all entities
        -- This function will either move an entity or delete it, depending on the result of tickEntity
    tickEntities :: [Entity] -> [Entity]
    -- Base case for list of new entities to be composed upon
    tickEntities [] = []
    -- Tick each entity and assess result
    tickEntities (e : es) = case (tickEntity e) of 
        -- If an entity has been returned from the function, it should be added to the new list 
        Just x -> x : tickEntities es
        -- Nothing indicates that the entity should be not added to the new list 
        Nothing -> tickEntities es


    -- Tick individual entity
        -- Returns maybe entity as the entity may be deleted
    tickEntity :: Entity -> Maybe Entity
    -- Return appended entity
    tickEntity e = entity 
        where
        -- All different cases for different types of enemy
        entity 
            -- If the entity has 0 or less health, the entity should not exist so return nothing
            | entityHealth e <= 0 = Nothing
            
            -- Enemy cases
                -- If the enemy intersects with the player, reduce entity health
            | entityType e == Enemy && intersect (x,y) (playerSize (player w)) (px,py) (entitySize e) = Just (e {entityHealth = entityHealth e - 20})
                -- If the enemy intersects with a bullet, reduce entity health
            | entityType e == Enemy && intersectEntities (x,y) (entitySize e) (bullets (entities w)) = Just (e {entityHealth = entityHealth e - 20})
                -- IF the enemy intersects with a supply package, the entity health should be doubled
            | entityType e == Enemy && intersectEntities (x,y) (entitySize e) (supplyPackages (entities w)) = Just (e {entityHealth = entityHealth e * 2})
            
            -- Bullet cases
                -- If the bullet intersects with an enemy, return nothing - the bullet should be deleted
            | entityType e == Bullet && intersectEntities (x,y) (entitySize e) (enemies (entities w)) = Nothing
                -- If the bullet has travelled more than 1200 pixels, return nothing 
            | entityType e == Bullet && distance (x,y) (startLocation $ entityInfo e) > 1200 = Nothing
                
            -- Supply package cases
                -- If the supply package intersects with an enemy, supply package should be deleted so Nothing
            | entityType e == SupplyPackage && intersectEntities (x,y) (entitySize e) (enemies (entities w)) = Nothing
                -- If the supply package intersects with the player, supply package should be deleted so Nothing
            | entityType e == SupplyPackage && intersect (x,y) (playerSize (player w)) (px,py) (entitySize e) = Nothing
            
            -- If none of these cases apply to the entity, the entity should move
            | otherwise = Just e {entityLocation = (x + diffx,y + diffy)}
                where 
                
                -- Define player location x and y coords
                (px, py) = playerLocation (player w)
                -- Define entity location x and y coords
                (x, y) = entityLocation e

                -- Choose the movement behavour of the entity based on the entity type
                (diffx, diffy) = case (entityType e) of 
                    Enemy -> (enemyDiffX, enemyDiffY)
                    Bullet -> (bulletDiffX, bulletDiffY)
                    SupplyPackage -> (0,0) -- Supply packages do not move


                -- The difference in the bullet depends on the direction and the speed of the bullet
                    -- Convert bullet direction and magnitude from polar form to cartesian form
                bulletDiffX = cos (direction (entityInfo e)) * bulletSpeed (entityInfo e) 
                bulletDiffY = sin (direction (entityInfo e)) * bulletSpeed (entityInfo e) 


                -- Difference in enemy location 
                    -- If the difference between the player x and the entity x is greater than the distance the entity can potentially move 
                enemyDiffX = if (abs (px - x) > enemySpeed (entityInfo e) ) 
                    -- If the entity is to the right of the player, move right, else move left
                    then (if ( px > x ) 
                        then (enemySpeed (entityInfo e) * ratiox) -- Multiplied by the ratio (ratio explanation below)
                        else (-enemySpeed (entityInfo e) * ratiox)
                    )
                    else 0
                enemyDiffY = if (abs (py - y) > enemySpeed (entityInfo e) ) 
                    then (if ( py > y ) 
                        then (enemySpeed (entityInfo e) * ratioy) 
                        else (-enemySpeed (entityInfo e) * ratioy)
                    )
                    else 0

                -- The ratio is so the enemy moves directly towards the player, rather than attempting
                    -- to align its x value and y value with the player at different rates   

                -- This is calculated by the absolute values of the differences in x or y divided by the differences in x+y
                ratiox = abs (px - x) / (abs (px - x) + abs (py - y))
                ratioy = abs (py - y) / (abs (px - x) + abs (py - y))
        







-- Drawing function
drawingFunc :: World -> Picture
-- Pattern match what should be drawn based on the game state
drawingFunc w 
    | gameState w == Title = Translate (-275) 0 $ Scale 0.12 0.25 -- Draw the title screen
        (Text "Zombie; press 'p' to start") 
    | gameState w == GameOver = Translate (-275) 0 $ Scale 0.12 0.25 -- Draw the game over screen
        (Text "Game over; press 'r' to return to title")
    | gameState w == Paused = Translate (-275) 0 $ Scale 0.12 0.25 -- Drawn the paused screen
        (Text "Game is paused; press 'p' to continue or 'r' to restart")
    | gameState w == Playing -- Draw the playing screen - this takes into account other attributes of the world record
        -- Put in pictures what needs to be rendered
        = Pictures [ -- This renders everything on the screen
            renderedStats -- render stats
            , playerMarker -- render the player
            , renderedEntities -- render all the enemies 
            ]
            where  



                --------------------------------------------------------
                -- STATS 


                -- Combined function for all of the stats to be rendered
                renderedStats = Pictures [levelStat, healthStat, enemiesRemainingStat]
                
                -- Stat for the game level
                levelStat = Translate (300) 300 $ Scale 0.12 0.25 (Text ("Level " ++ (show $ level (worldStats w))) )
                -- Stat for enemies remaining
                enemiesRemainingStat = Translate (300) 230 $ Scale 0.12 0.25 (Text ("Enemies: " ++ (show $ length $ enemies (entities w))) )
                -- Stat for health bar
                healthStat = Pictures [emptyBar, currentHealthBar]
                    where 
                        -- Render the empty health bar below
                        emptyBar = 
                            let (centerX, centerY) = (300, 270) -- Center of the health bar
                                -- Define the edges of the health bar
                                tl = (centerX - 50, centerY + 5) -- top left
                                tr = (centerX + 50, centerY + 5) -- top right
                                br = (centerX + 50, centerY - 5) -- bottom right
                                bl = (centerX - 50, centerY - 5) -- bottom left
                                -- Render a red polygon with those corners
                            in  Color red (Polygon [tl, tr, br, bl])
                        currentHealthBar = 
                            let (centerX, centerY) = (250, 270) 
                                tl = (centerX, centerY + 5)
                                -- For the right side, render as far as many pixels as the user has health
                                tr = (centerX + playerHealth (player w), centerY + 5)
                                br = (centerX + playerHealth (player w), centerY - 5)
                                bl = (centerX, centerY - 5)
                            in  Color green (Polygon [tl, tr, br, bl])

                
                --------------------------------------------------------



                --------------------------------------------------------
                -- Player 

                -- Render the player marker
                    -- Very similar rendering method as seen above
                playerMarker = 
                    let (px, py) = playerLocation $ player w 
                        -- The size here is technically the distance from the center of the polygon to the center of any one of the sides
                        size = playerSize (player w) 
                        tl = (px - (size), py + (size))
                        tr = (px + (size), py + (size))
                        br = (px + (size), py - (size))
                        bl = (px - (size), py - (size))
                    in  Color cyan (Polygon [tl, tr, br, bl])

                --------------------------------------------------------


                --------------------------------------------------------
                -- Entities 

                -- Function for all entities to be rendered
                    -- Contains pictures of bullets, enemies and supply packages
                renderedEntities = Pictures [Pictures (bulletPics), Pictures (enemyPics), Pictures (supplyPackagePics)]
                    where 
                        -- Fmap the bulletPic function onto all bullets (same for the enemypic and supplypackagepic)
                        bulletPics = bulletPic <$> (bullets (entities w))
                        enemyPics = enemyPic <$> (enemies (entities w))
                        supplyPackagePics = supplyPackagePic <$> (supplyPackages (entities w))


                -- Function to render a bullet
                bulletPic :: Entity -> Picture
                bulletPic b = 
                    -- The location of the bullet will be the center of the image rendered
                    let (centerX, centerY) = entityLocation b
                        size = entitySize b
                        tl = (centerX - (size), centerY + (size)) -- top left of bullet
                        tr = (centerX + (size), centerY + (size)) -- top right
                        br = (centerX + (size), centerY - (size)) -- bottom right
                        bl = (centerX - (size), centerY - (size)) -- bottom left
                    in  Color blue (Polygon [tl, tr, br, bl])


                -- Function to render an enemy
                enemyPic :: Entity -> Picture
                enemyPic e =
                    let (centerX, centerY) = entityLocation e
                        size = entitySize e
                        tl = (centerX - (size), centerY + (size))
                        tr = (centerX + (size), centerY + (size))
                        br = (centerX + (size), centerY - (size))
                        bl = (centerX - (size), centerY - (size))
                    in  Color c (Polygon [tl, tr, br, bl])
                        where 
                            c = (makeColor redFactor 0 0 1)
                            redFactor = if (1 - (entityHealth e) / 500 > 0) 
                                then (1 - (entityHealth e) / 500)
                                else 0

                
                -- Function to render a supply package
                supplyPackagePic :: Entity -> Picture
                supplyPackagePic p =
                    let (centerX, centerY) = entityLocation p
                        size = entitySize p
                        tl = (centerX - (size), centerY + (size))
                        tr = (centerX + (size), centerY + (size))
                        br = (centerX + (size), centerY - (size))
                        bl = (centerX - (size), centerY - (size))
                    in  Color green (Polygon [tl, tr, br, bl])



  
