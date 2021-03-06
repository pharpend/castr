{-|
Module       : Math.Castr.Mate
Description  : Describes the mating of Agents
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Math.Castr.Mate where

import           Control.Applicative
import           Data.Aeson
import           Data.Ord
import           Math.Castr.Agent

type Pair x = (x, x)
type Parent = Agent
type Child = Agent

data Mating = Mating { parents :: Pair Parent
                     , child   :: Child
                     }
  deriving (Eq, Show)

instance Ord Mating where
  compare = comparing child
  
instance FromJSON Mating where
  parseJSON (Object v) = Mating
    <$> v .: "parents"
    <*> v .: "child"
  parseJSON _ = fail "Mating must be an object."

instance ToJSON Mating where
  toJSON (Mating ps cs) = object [ "parents" .= ps
                                 , "child" .= cs
                                 ]
