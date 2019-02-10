# Haskade

## Background
This is an implementation of the classic arcade game Blockade released by Gremlin in 1976. Here is a [video](https://www.youtube.com/watch?v=5v-0CwiabZA) that shows the original. I implemented a simple version of this game in Haskell as a school assignment.

## Dependencies
To run the game you need GHC and an installation of ncurses. If you have GHC installed you can run the command `cabal intsall ncurses` to get ncurses and all its dependencies.

## How to run
Clone the repository and navigate to the folder. Entering the command `runhaskell main.hs` will start the game.

## Controls
#### Player 1: W, A, S, D
#### Player 2: I, J, K, L
You can press ESC at any time to exit the game.

## Rules:
Any time a player collides with a wall, himself or the other player, the other player gets a point. The first player to reach 5 points wins the game.
