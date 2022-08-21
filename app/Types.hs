module Types where

import System.Random (StdGen)

import Data.Map (Map)
import qualified Data.Map as Map


------------------------------------------------
-- Types

type Location = (Float, Float)

type Angle = Float

type Health = Float

type Size = Float

type KeyBoardInput = Map String Bool




------------------------------------------------
-- Data


-- World information stored - important, passed between most functions
data World = World
  { player                :: Player           -- All player information stored in this field
  , gameState             :: GameState        -- Game state information
  , worldRandomGenerator  :: StdGen           -- Random generator for the world
  , entities              :: [Entity]         -- List of all entities
  , worldStats            :: Stats            -- All world stats
  , keyBoardInput         :: KeyBoardInput    -- Keyboard input information (that needs to be stored)
  }


data Player = Player              -- Record fields self documenting                      
  { playerLocation        :: Location        
  , playerHealth          :: Health            
  , playerSize            :: Size
  }





-- Current state of the game - indicates what should be shown/updated
data GameState = Title | Playing | Paused | GameOver
    deriving (Show, Eq)


-- Information about a particular entity
data Entity = Entity 
  { entityType            :: EntityType   -- Type of entity
  , entityHealth          :: Health       -- Entity health
  , entitySize            :: Size         -- Entity size
  , entityLocation        :: Location     -- Location of entity
  , entityInfo            :: EntityInfo   -- Information about entity (this depends on the entity type)
  }


-- What type of entity an entity is 
data EntityType = 
      Enemy 
    | Bullet 
    | SupplyPackage
    deriving (Show, Eq)


-- Information about an entity specific to an entity
  -- This data type is for the entity record, so that useless information isn't stored about an entity that has no relevance
  -- E.g. start location for a supply package - it isn't going anywhere!!!
data EntityInfo = 
    EnemyInfo 
      {
        enemySpeed   :: Float
      }
    | 
    BulletInfo 
      {
        startLocation   :: Location
      , direction       :: Angle
      , bulletSpeed   :: Float
      } 
    | SupplyPackageInfo
      
    deriving (Show, Eq)



-- Stats stored here (I thought there would be more stats but... well I just didn't have the time to implement)
data Stats = Stats 
  { level :: Int
  }



