# Haskade

## Background
This is an implementation of the classic arcade game Blockade released by Gremlin in 1976. Here is a [video](https://www.youtube.com/watch?v=5v-0CwiabZA) that shows the original. I made this as a part of a school assignment for the course
[Programming Paradigms DD1362](https://www.kth.se/social/course/DD1362/).

## Dependencies
To run the game you need GHC and an installation of ncurses. If you have GHC installed you can run the command `cabal intsall ncurses` to get ncurses and all its dependencies.

## How to run
Clone the repository and navigate to the folder. Entering the command `runhaskell main.hs` will start the game.
On a default terminal the movement in the x-direction will be faster than the movement in the y-direction.
This is not a disaster but it is not how the game is made to be played. Tweaking your terminals line space and
character space settings until the playing field has the shape of a square is a good fix.

## Controls
#### Player 1 (Red): W, A, S, D
#### Player 2 (Blue): I, J, K, L
You can press ESC to exit the game.

## Rules:
Any time a player collides with a wall, himself or the other player, the other player gets a point. The first player to reach 5 points wins the game.
