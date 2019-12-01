module Main where

import Rendering
import Logic
import Game

import Graphics.Gloss
import Graphics.Gloss.Data.Color


window = InWindow "A Game" (screenWidth, screenHeight) (100, 100)


main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
