{-# Language DeriveGeneric #-}
module Main where

import Rendering
import Logic
import Game

import Debug.Trace (trace)
import Control.Lens hiding (Empty)

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game

import Network.WebSockets hiding (Message)
import Network.Socket (withSocketsDo)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.Environment
import System.Exit
import GHC.Generics
import Data.Serialize (Serialize, encodeLazy, decodeLazy)

data AppMode = Standalone | Server Int | Client String Int deriving (Show, Read, Eq)

data Message = NoOp | Click Player (Float, Float) deriving (Show, Read, Generic)

instance Serialize Message
instance Serialize Player

fromRight (Right a) = a --FIXME ignore, report or something
fromRight _ = error "deserialization error. sorry :("

instance WebSocketsData Player where
  fromLazyByteString = fromRight . decodeLazy
  toLazyByteString = encodeLazy
  fromDataMessage (Binary bs) = fromLazyByteString bs
  fromDataMessage _ = error "Invalid websocket message"
instance WebSocketsData Message where
  fromLazyByteString = fromRight . decodeLazy
  toLazyByteString = encodeLazy
  fromDataMessage (Binary bs) = fromLazyByteString bs
  fromDataMessage _ = error "Invalid websocket message"



liftM'::Monad m=>(a->b)->(a->m b)
liftM' f = (\x -> return $ f x)
liftM''::Monad m=>(a->b->c)->(a->b->m c)
liftM'' f = (\x y -> return $ f x y)

ws :: MVar Message -> Chan Message -> Connection -> IO ()
ws msgM sC conn = do
  putStrLn "in ws"
  _ <- forkIO $ recvData conn  msgM
  sendData conn sC

sendData :: Connection -> Chan Message -> IO ()
sendData conn ch = do
  putStrLn "in senddata"
  forever $ do
    m <- readChan ch
    sendBinaryData conn m
  --hClose hdl

recvData :: Connection ->  MVar Message -> IO ()
recvData conn msgM = do
  putStrLn "in recvData"
  forever $ do
    msg <- receiveData conn
    putMVar msgM msg
    -- modifyMVar_ msgM (\_->return msg)
  --hClose hdl
  --
  --
  --

otherPlayer PlayerO = PlayerX
otherPlayer PlayerX = PlayerO


step :: MVar Message -> Chan Message -> Float -> Game -> IO Game
step msgM sCh dt game = do
  msg <- tryTakeMVar msgM
  case msg of
    Nothing -> return game
    Just NoOp -> return game
    Just (Click p (x,y)) -> do  
       putStrLn $ "in step"++(show (game^.iam, game^.player, p, (game & player .~ p)^.player))
       let game' = transformGame (EventKey (MouseButton LeftButton) Up (undefined) (x,y)) (game & player .~ p)
       return game'

handleInput :: Chan Message -> Event -> Game -> IO Game
handleInput sCh evt@(EventKey (MouseButton LeftButton) Up _ mousePos) game = do
  if game^.iam == game^.player  
  then do 
       writeChan sCh (Click (game^.iam) mousePos)
       return $  transformGameCS evt game 
  else return game 
        
handleInput _ _ game = return game

main :: IO ()
main = do
    args <- getArgs
    appMode <- case args of "client":ip:port:[] -> return $ Client ip (read port)
                            "server":port:[] -> return $ Server (read port)
                            "standalone":[] -> return Standalone
                            _ -> do putStrLn "Invalid arguments."
                                    putStrLn "Usage:"
                                    putStrLn "  tic-tac-toe client address port      - connects to an existing server"
                                    putStrLn "  tic-tac-toe server port              - creates a server"
                                    exitFailure

    let iam = case appMode of Server _ -> PlayerX; Client _ _ -> PlayerO;
    let window = InWindow ("A Game "++(show iam)) (screenWidth, screenHeight) (100, 100)

    if appMode /= Standalone
    then do
       sendChan <- newChan
       msgM <- newMVar NoOp
       _ <- forkIO $ case appMode of
            Server port -> do
                putStrLn "starting server .." 
                runServer "0.0.0.0" port $ ws msgM sendChan <=< acceptRequest
                putStrLn "started server .." 
            Client ip port -> do
                putStrLn "starting client .." 
                withSocketsDo $ runClient ip port "/" $ ws msgM sendChan
                putStrLn "started client .." 
       playIO window backgroundColor 30 (initialGame iam) 
              (liftM' gameAsPicture) 
              (handleInput sendChan)
              (step msgM sendChan)
       putStrLn "TODO clean up sockets" 
    else
        playIO window backgroundColor 30 (initialGame PlayerX) 
                (liftM' gameAsPicture) 
                (liftM'' transformGame)
                (liftM'' (const id))
