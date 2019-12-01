module Logic where

import Debug.Trace (trace)

import Game
import Rendering (screenWidth, screenHeight, scaleHeight, scaleWidth)
import Data.Array
import Data.Maybe
import Control.Lens hiding (Empty)
import Graphics.Gloss.Interface.Pure.Game
                

playerAction::Game->(Int,Int)->Game
playerAction game (x,y) = (game & board .~ ((game^.board) // [((x,y),(OccupiedBy $ game^.player))]))

nextTurn::Game->Game
nextTurn game 
    | game^.player == PlayerO = game & player .~ PlayerX
    | game^.player == PlayerX = game & player .~ PlayerO

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

         

mousePosToBoardCoord::(Float,Float)->(Int,Int)
mousePosToBoardCoord (x',y') = (max 1 $ min n $ x, max 1 $ min n $ y)
 where x = 1 + (floor $ (fromIntegral n)*(x' + scaleWidth/2)/(scaleWidth))
       y = n - (floor $ (fromIntegral n)*(y' + scaleHeight/2)/(scaleHeight))

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case game^.state of
        (GameOver _)  -> initialGame
        Running       -> checkOutcome
                         . nextTurn
                         . playerAction game
                         . mousePosToBoardCoord
                         $ mousePos
        
transformGame _ game = game
