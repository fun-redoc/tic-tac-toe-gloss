{-# Language DeriveGeneric #-}
module Main where

import Rendering
import Game
import Communication

import Debug.Trace (trace)
import Graphics.Gloss.Interface.IO.Game

import Network.WebSockets hiding (Message)
import Network.Socket (withSocketsDo)
import Control.Concurrent
import Control.Monad
import System.Environment
import System.Exit

data AppMode = Standalone | Server Int | Client String Int deriving (Show, Read, Eq)

main :: IO ()
main = do
    args <- getArgs
    appMode <- case args of "client":ip:port:[] -> return $ Client ip (read port)
                            "server":port:[] -> return $ Server (read port)
                            "standalone":[] -> return Standalone
                            _ -> do putStrLn "Invalid arguments."
                                    putStrLn "Usage:"
                                    putStrLn "  tic-tac-toe client address port  - connects to a server"
                                    putStrLn "  tic-tac-toe server port          - creates a server"
                                    putStrLn "  tic-tac-toe standalone           - one player standalone game"
                                    exitFailure

    let iam = case appMode of Server _ -> PlayerX; Client _ _ -> PlayerO; _ -> PlayerX
    let window = InWindow ("Tic Tac Toe "++(show iam)) (screenWidth, screenHeight) (100, 100)

    if appMode /= Standalone
    then do
       -- 2 players
       sendChan <- newChan
       msgM <- newMVar NoOp
       _ <- forkIO $ case appMode of
            Server port -> do
                runServer "0.0.0.0" port $ communicate msgM sendChan <=< acceptRequest
                putStrLn "started server .." 
            Client ip port -> do
                withSocketsDo $ runClient ip port "/" $ communicate msgM sendChan
                putStrLn "started client .." 
       playIO window backgroundColor 30 (initialGame iam) 
              (liftM' gameAsPicture) 
              (handleInputRemote sendChan)
              (update msgM sendChan)
    else
       -- single player
       playIO window backgroundColor 30 (initialGame PlayerX) 
               (liftM' gameAsPicture) 
               (liftM'' handleInput)
               (liftM'' (const id))
