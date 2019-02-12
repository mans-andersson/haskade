{-Written by Måns Andersson-}
module Main where

import UI.NCurses
import Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import Control.Monad.IO.Class(liftIO)
import Data.Char(toUpper)
import System.Exit(exitSuccess)
import Control.Concurrent(threadDelay)
import Data.List
import Game

main = runCurses $ do
  setEcho False
  w <- newWindow (rows+1) (columns+20) 0 0
  gameLoop w initialGameState

{-The main gameloop!-}
gameLoop :: Window -> GameState -> Curses ()
gameLoop w gs = do
  red <- newColorID ColorRed ColorDefault 1
  blue <- newColorID ColorBlue ColorDefault 2
  white <- newColorID ColorWhite ColorDefault 3
  {-This part renders everything.-}
  updateWindow w $ do
    setColor white
    sequence drawWalls
    setColor red
    sequence (drawPlayer (getP1 gs) 'x')
    setColor blue
    sequence (drawPlayer (getP2 gs) 'o')
    setColor white
    drawScores (getScore gs)
  render
  checkWinScore w gs
  checkState w gs
  {-Reads the current time from the system. We need this to control the
    pace of the game.-}
  currenttime <- liftIO getPOSIXTime
  {-Halts for 1 ms looking for events. If these are any relevant button
    presses they will affect the game.-}
  e <- getEvent w (Just 1)
  gameLoop w $ updateGameState gs e currenttime

waitForSpace :: Window -> Curses ()
waitForSpace w = loop
  where loop = do
        e <- getEvent w Nothing
        case e of
          Just (EventCharacter ' ') -> return ()
          _ -> loop

{-If a player has reached the winningScore the inner state is changed.-}
checkWinScore :: Window -> GameState -> Curses ()
checkWinScore w gs
  | p1score == winningScore = checkState w $ changeState Player1Won gs
  | p2score == winningScore = checkState w $ changeState Player2Won gs
  | otherwise = return ()
   where (p1score, p2score) = getScore gs

{-Checks the inner state of the GameState and acts accordingly.-}
checkState :: Window -> GameState -> Curses()
checkState w gs
  | state == Running = return ()
  | state == Player1Coll = do
      drawMessage "PLAYER 2 GOT A POINT!" w
      delaySeconds 2 w
      updateWindow w clear
      gameLoop w $ GameState Running initialP1 initialP2 0 (getScore gs)
  | state == Player2Coll = do
      drawMessage "PLAYER 1 GOT A POINT!" w
      delaySeconds 2 w
      updateWindow w clear
      gameLoop w $ GameState Running initialP1 initialP2 0 (getScore gs)
  | state == DoubleColl = do
      drawMessage "YOU BOTH GOT A POINT!" w
      delaySeconds 2 w
      updateWindow w clear
      gameLoop w $ GameState Running initialP1 initialP2 0 (getScore gs)
  | state == Player1Won = do
      drawMessage "PLAYER 1 WON THE GAME!" w
      delaySeconds 3 w
      updateWindow w clear
      gameLoop w $ initialGameState
  | state == Player2Won = do
      drawMessage "PLAYER 2 WON THE GAME!" w
      delaySeconds 3 w
      updateWindow w clear
      gameLoop w $ initialGameState
  | state == Draw = do
      drawMessage "NOBODY WON! IT IS A DRAW!" w
      delaySeconds 3 w
      updateWindow w clear
      gameLoop w $ initialGameState
  | state == Quit = liftIO exitSuccess
  | state == MainMenu = do
      updateWindow w clear
      drawMessage "WELCOME TO HASKADE! \n PRESS SPACE TO START" w
      waitForSpace w
      gameLoop w $ changeState Running gs
    where state = getState gs

{-Displays a message on the screen for a specified amount of seconds.
  During this time the gameplay is frozen.-}
drawMessage :: String -> Window -> Curses ()
drawMessage m w = do
  updateWindow w $ do
    moveCursor (centerY-5) (centerX - ((genericLength m) `div`2))
    drawString m
  render
  updateWindow w clear

{-Uses the getEvent function in ncurses to simply delay the game.
  Used for messages.-}
delaySeconds :: Integer -> Window -> Curses ()
delaySeconds s w = do
  liftIO $ threadDelay (fromIntegral time)
  return ()
    where time = s * 1000000

{-Calls a composition of functions that changes the gamestate in some way.-}
updateGameState :: GameState -> Maybe Event -> POSIXTime -> GameState
updateGameState gs e ptime = (readEvent e) . (moveGame time) $ gs
  where time = getMilliSeconds ptime


{-Transforms the POSIXTime timestamp to a millisecond value.
  Used to control the pace of the game.-}
getMilliSeconds :: POSIXTime -> Timestamp
getMilliSeconds t = round $ t * 1000

{-Checks if the event is a relevant button press and performs the relevant
  action.-}
readEvent :: Maybe Event -> GameState -> GameState
readEvent (Just (EventCharacter k)) gs@(GameState s p1 p2 ts sc)
  | k `isKey` 'w' = GameState s (changeDirection p1 Game.Up) p2 ts sc
  | k `isKey` 'a' = GameState s (changeDirection p1 Game.Left) p2 ts sc
  | k `isKey` 's' = GameState s (changeDirection p1 Game.Down) p2 ts sc
  | k `isKey` 'd' = GameState s (changeDirection p1 Game.Right) p2 ts sc
  | k `isKey` 'i' = GameState s p1 (changeDirection p2 Game.Up) ts sc
  | k `isKey` 'j' = GameState s p1 (changeDirection p2 Game.Left) ts sc
  | k `isKey` 'k' = GameState s p1 (changeDirection p2 Game.Down) ts sc
  | k `isKey` 'l' = GameState s p1 (changeDirection p2 Game.Right) ts sc
  | k `isKey` '\ESC' = changeState Quit gs
    where isKey k i = k == i || k == (toUpper i)
readEvent _ gs = gs

{-Drawing functions.-}

{-Draws a specified char at specified coordinates.-}
drawChar :: Char -> Integer -> Integer -> Update ()
drawChar ch row col = do
  moveCursor row col
  drawString [ch]

{-Draws a specified char at the location of a specified PBlock.-}
drawPBlock :: Char -> PBlock -> Update ()
drawPBlock ch pb = drawChar ch (yCoord pb) (xCoord pb)

{-Draws the walls based on the constants rows and columns in the Game module.-}
drawWalls :: [Update ()]
drawWalls = leftright ++ topbottom
  where leftright = (drawChar '#') <$> [0..rows-1] <*> [0,columns-1]
        topbottom = (drawChar '#') <$> [0,rows-1] <*> [0..columns-1]

{-Draws all the PBlocks that form the Player. The head gets drawn differently
  (as an arrow) than the rest of the Player.-}
drawPlayer :: Player -> Char -> [Update ()]
drawPlayer [] _ = []
drawPlayer (h:t) ch = (drawPBlock dirChar h):(drawPlayerTail t)
  where drawPlayerTail t = map (drawPBlock ch) t
        dirChar = getDirectionChar(getDir h)

{-Draws the scores at the right side of the playing field.-}
drawScores :: Score -> Update ()
drawScores (p1, p2) = do
  moveCursor 2 (columns+2)
  drawString ("Player 1: " ++ (show p1))
  moveCursor 3 (columns+2)
  drawString ("Player 2: " ++ (show p2))
