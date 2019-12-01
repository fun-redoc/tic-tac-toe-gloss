module Rendering where

import Debug.Trace (trace)
import Control.Lens hiding (Empty)
import Graphics.Gloss       
import Data.Array

import Game

screenWidth = 640::Int
screenHeight = 400::Int


scaleWidth  = fromIntegral $ min screenWidth screenHeight
scaleHeight = fromIntegral $ min screenWidth screenHeight

backgroundColor = makeColor 0 0 0 255

playerColor PlayerX = makeColorI 255  50  50 255
playerColor PlayerO = makeColorI  50 100 255 255
gridColor = makeColorI 255 255 50 255
tieColor     = greyN 0.5

outcomeColor Tie = tieColor
outcomeColor (Winner p) = playerColor p

scaleIt = scale (scaleWidth/(fromIntegral n)) (scaleHeight/(fromIntegral n)) 

grid board = scaleIt $ color gridColor $ pictures [h1,h2,v1,v2]
    where
        h1 = line [(-0.5,1.5), (-0.5,-1.5)]
        h2 = line [(0.5,1.5), (0.5,-1.5)]
        v1 = line [(-1.5,0.5), (1.5,0.5)]
        v2 = line [(-1.5,-0.5), (1.5,-0.5)]

cellPicture ((x,y), cell)
    = case cell of
        Empty -> Blank
        OccupiedBy PlayerX -> scaleIt
                              $ translateIt x y
                              $ pictures [rotate 45.0 bar, rotate (-45.0) bar]
        OccupiedBy PlayerO -> scaleIt 
                              $ translateIt x y
                              $ thickCircle radius thickness
       where
        radius = 0.3
        thickness = 0.1
        bar = polygon [(-0.3,0.05),(0.3,0.05),(0.3,-0.05), (-0.3,-0.05)]
        translateIt x y = translate ((fromIntegral (x-2))) ((fromIntegral (2-y)))

cellPicturWithColor cell@(_, OccupiedBy player) 
    = color (playerColor player) $ cellPicture cell
cellPicturWithColor cell@(_, Empty) = cellPicture cell

playerCells state board 
    = case state of 
        (GameOver _) -> pictures. map cellPicture . assocs $ board
        (Running) -> pictures . map cellPicturWithColor . assocs $ board

boardAsPicture::State -> Board -> Picture
boardAsPicture state@(GameOver winner) board 
    = color (outcomeColor winner) $  (pictures [playerCells state board, grid board])
boardAsPicture state@Running board  
    = pictures [ playerCells state board
               , grid board
               ]

gameAsPicture::Game->Picture
gameAsPicture game = boardAsPicture (game^.state) (game^.board)
