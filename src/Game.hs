{-# LANGUAGE TemplateHaskell #-}
module Game where
import Debug.Trace (trace)
import Control.Lens hiding (Empty)
import Data.Array

data Cell  = Empty | OccupiedBy Player deriving (Eq, Show)
instance Semigroup Cell where
    (<>) Empty _ = Empty
    (<>) _ Empty = Empty
    (<>) (OccupiedBy p1) (OccupiedBy p2) = if p1 == p2 then (OccupiedBy p1) else Empty
type Board = Array (Int, Int) Cell 
data Player = PlayerX | PlayerO deriving (Eq, Show)
data Winner = Winner Player | Tie deriving (Eq, Show)
data State = Running | GameOver Winner deriving (Eq, Show) 

data Game = Game { _board::Board
                 , _player::Player
                 , _state::State
                 } deriving (Eq, Show)

makeLenses ''Game

n::Int
n = 3

initialGame = Game 
  { _board = array indexRange $ zip (range indexRange) (repeat (Empty)) 
  , _player = PlayerO
  , _state  = Running -- (GameOver $ Just PlayerO)
  }
  where indexRange = ((1,1),(n,n))
