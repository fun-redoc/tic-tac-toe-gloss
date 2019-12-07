{-# Language DeriveGeneric #-}
module Communication where
import Game
import Rendering (handleInput)

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


data Message = NoOp | Click Player (Float, Float) deriving (Show, Read, Generic)

instance Serialize Message
instance Serialize Player

fromRight (Right a) = a 
fromRight _ = error "error in deserialization"

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

communicate :: MVar Message -> Chan Message -> Connection -> IO ()
communicate msgM sC conn = do
                              _ <- forkIO $ recvData conn  msgM
                              sendData conn sC

sendData :: Connection -> Chan Message -> IO ()
sendData conn ch = do
  forever $ do
    m <- readChan ch
    sendBinaryData conn m

recvData :: Connection ->  MVar Message -> IO ()
recvData conn msgM = do
  forever $ do
    msg <- receiveData conn
    putMVar msgM msg



update :: MVar Message -> Chan Message -> Float -> Game -> IO Game
update msgM sCh dt game = do
  msg <- tryTakeMVar msgM
  case msg of
    Nothing -> return game
    Just NoOp -> return game
    Just (Click p (x,y)) -> do  
       let game' = handleInput (EventKey (MouseButton LeftButton) Up (undefined) (x,y)) (game & player .~ p)
       return game'

handleInputRemote :: Chan Message -> Event -> Game -> IO Game
handleInputRemote sCh evt@(EventKey (MouseButton LeftButton) Up _ mousePos) game = do
  if game^.iam == game^.player  
  then do 
       writeChan sCh (Click (game^.iam) mousePos)
       return $  if game^.player /= game^.iam then game else handleInput evt game 
  else return game 
        
handleInputRemote _ _ game = return game

