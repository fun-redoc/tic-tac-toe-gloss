{-# LANGUAGE TemplateHaskell #-}
{-# Language DeriveGeneric #-}
module Game where
import Debug.Trace (trace)
import Control.Lens hiding (Empty)
import Data.Array
import GHC.Generics


data Cell  = Empty | OccupiedBy Player deriving (Eq, Show)
instance Semigroup Cell where
    (<>) Empty _ = Empty
    (<>) _ Empty = Empty
    (<>) (OccupiedBy p1) (OccupiedBy p2) = if p1 == p2 then (OccupiedBy p1) else Empty
type Board = Array (Int, Int) Cell 
data Player = PlayerX | PlayerO deriving (Eq, Show, Read, Generic)
data Winner = Winner Player | Tie deriving (Eq, Show)
data State = Running | GameOver Winner deriving (Eq, Show) 

data Game = Game { _board::Board
                 , _iam::Player
                 , _player::Player
                 , _state::State
                 } deriving (Eq, Show)

makeLenses ''Game

n::Int
n = 3

initialGame iam = Game 
  { _board = array indexRange $ zip (range indexRange) (repeat (Empty)) 
  , _iam = iam
  , _player = PlayerO
  , _state  = Running -- (GameOver $ Just PlayerO)
  }
  where indexRange = ((1,1),(n,n))

 
                

playerAction::Game->(Int,Int)->Game
playerAction game (x,y) = (game & board .~ ((game^.board) // [((x,y),(OccupiedBy $ game^.player))]))

nextTurn::Game->Game
nextTurn game 
    | game^.player == PlayerO = game & player .~ PlayerX
    | game^.player == PlayerX = game & player .~ PlayerO

otherPlayer PlayerO = PlayerX
otherPlayer PlayerX = PlayerO

checkOutcome::Game->Game
checkOutcome game = game & state .~ state'
  where 
   state' = if gameOver then (GameOver $ winner) else (game^.state)
   board' = game^.board
   gameOver = all isOccupied board' || winner' PlayerX || winner' PlayerO
   winner = if winner' PlayerO 
            then Winner PlayerO 
            else if winner' PlayerX 
                 then Winner PlayerX 
                 else Tie
   winner' p = (any (hasRow p) [1..n]) 
               || (any (hasCol p) [1..n])
               || (hasDiag1 p)
               || (hasDiag2 p)
   hasRow p y = foldl (\f x->f && ((OccupiedBy p)==(board'!(x,y))))     True [1..n] 
   hasCol p x = foldl (\f y->f && ((OccupiedBy p)==(board'!(x,y))))     True [1..n] 
   hasDiag1 p = foldl (\f y->f && ((OccupiedBy p)==(board'!(y,y))))     True [1..n] 
   hasDiag2 p = foldl (\f y->f && ((OccupiedBy p)==(board'!(n-y+1,y)))) True [1..n] 
   isOccupied Empty = False
   isOccupied (OccupiedBy _) = True


