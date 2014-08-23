{-|
Module       : Math.Castr.Agent
Description  : Code for agents
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Math.Castr.Agent where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Vector (Vector, empty, singleton)
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

-- |Pools are vectors of agents
type Pool = Vector Agent

-- |Agents have an attribute
newtype Agent = Agent { attribute :: Attribute }
  deriving (Eq, Show)

-- |Attributes are numerical, with a genetic and environmental component
data Attribute = Attribute { genPart :: Double
                           , envPart :: Double
                           }
  deriving (Eq, Show)

-- |Sum the components
attributeValue :: Attribute -> Double
attributeValue (Attribute g e) = g + e

-- |Attributes are ordered by their total value
instance Ord Attribute where
  compare = comparing attributeValue

-- |Randomly generate a normal attribute, given a heritability
genAttribute :: Double       -- ^Heritability (0 < h < 1)
             -> IO Attribute -- ^Resultant attribute
genAttribute h = Attribute <$> geneticComponent <*> environmentalComponent
  where
    geneticComponent :: IO Double
    geneticComponent = genContVar (normalDistr 0 $ sqrt h) =<< createSystemRandom

    environmentalComponent :: IO Double
    environmentalComponent = genContVar (normalDistr 0 . sqrt $ 1 - h) =<< createSystemRandom

-- |Randomly generate a normal Agent
genAgent :: Double   -- ^Heritability (0 < h < 1)
         -> IO Agent -- ^Resultant attribute
genAgent = fmap Agent . genAttribute

-- |Generate a pool
mkPool :: Int     -- ^Number of agents in the pool <0
       -> Double  -- ^Heritability 0 < h < 1
       -> IO Pool -- ^The resultant pool
mkPool n h = addToPool n h empty

-- |Add some members to an existent pool
addToPool :: Int     -- ^Number of agents to add pool <0
          -> Double  -- ^Heritability 0 < h < 1
          -> Pool    -- ^Pool to add to
          -> IO Pool -- ^The resultant pool
addToPool 0 _ p = return p
addToPool n h p = addToPool (n-1) h =<< addOne
  where
    addOne :: IO Pool
    addOne = mappend p . singleton <$> genAgent h


-- JSON interface
instance FromJSON Attribute where
  parseJSON (Object v) = Attribute <$> v .: "genetic-part"
                                   <*> v .: "environmental-part"
  parseJSON _ = fail "Not an object"

instance ToJSON Attribute where
  toJSON (Attribute g e) = object [ "genetic-part" .= g
                                  , "environmental-part" .= e
                                  ]

instance FromJSON Agent where
  parseJSON = fmap Agent . parseJSON

instance ToJSON Agent where
  toJSON (Agent attr) = toJSON attr
