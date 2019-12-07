# tic-tac-toe-gloss

simple tic tac toe game for two remote players written in haskell

used gloss for rendering
websocktes for communication


## Compile 

stack build

## Run Standalone

stack run -- standalone

## Run Two Player over IP

Player 1 (servermode): stack run -- server <port>
Player 2 (clientmode): stack run -- client <ip> <port>

## Help

stack run

shows arguments help

## Compile Run Loop

stack build --file-watch --exec "bash -c \"pkill tic-tac-toe-gloss; stack run &\""

