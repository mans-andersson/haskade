import UI.NCurses
import Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import Control.Monad.IO.Class(liftIO)
import Data.Char(toUpper)
import Game

main = runCurses $ do
  setEcho False
  w <- newWindow (rows+1) (columns+20) 0 0
  gameLoop w initialGameState

{-The main gameloop!-}
gameLoop :: Window -> GameState -> Curses ()
gameLoop w gs@(GameState p1 p2 ts sc) = do
  {-This part renders everything.-}
  updateWindow w $ do
    sequence drawWalls
    sequence (drawPlayer p1)
    sequence (drawPlayer p2)
    drawScores sc
  render
  {-Reads the current time from the system. We need this to control the
    pace of the game.-}
  currenttime <- liftIO getPOSIXTime
  {-Halts for 1 ms looking for events. If these are any relevant button
    presses they will affect the game.-}
  e <- getEvent w (Just 1)
  gameLoop w $ updateGameState gs e currenttime

{-Calls a composition of functions that changes the gamestate in some way.-}
updateGameState :: GameState -> Maybe Event -> POSIXTime -> GameState
updateGameState gs e ptime = (readEvent e) . (moveGame time) $ gs
  where time = getMilliSeconds ptime

drawChar :: Char -> Integer -> Integer -> Update ()
drawChar ch row col = do
  moveCursor row col
  drawString [ch]

drawPBlock :: Char -> PBlock -> Update ()
drawPBlock ch pb = drawChar ch (yCoord pb) (xCoord pb)

drawWalls :: [Update ()]
drawWalls = leftright ++ topbottom
  where leftright = (drawChar '#') <$> [0..rows-1] <*> [0,columns-1]
        topbottom = (drawChar '#') <$> [0,rows-1] <*> [0..columns-1]

drawPlayer :: Player -> [Update ()]
drawPlayer [] = []
drawPlayer (h:t) = (drawPBlock dirChar h):(drawPlayerTail t)
  where drawPlayerTail t = map (drawPBlock '#') t
        dirChar = getDirectionChar(getDir h)

drawScores :: Score -> Update ()
drawScores (p1, p2) = do
  moveCursor 2 (columns+2)
  drawString ("Player 1: " ++ (show p1))
  moveCursor 3 (columns+2)
  drawString ("Player 2: " ++ (show p2))

getMilliSeconds :: POSIXTime -> Timestamp
getMilliSeconds t = round $ t * 1000

{-Checks if the event is a relevant button press and performs the relevant
  action.-}
readEvent :: Maybe Event -> GameState -> GameState
readEvent (Just (EventCharacter k)) gs@(GameState p1 p2 ts sc)
  | k `isKey` 'w' = GameState (changeDirection p1 Game.Up) p2 ts sc
  | k `isKey` 'a' = GameState (changeDirection p1 Game.Left) p2 ts sc
  | k `isKey` 's' = GameState (changeDirection p1 Game.Down) p2 ts sc
  | k `isKey` 'd' = GameState (changeDirection p1 Game.Right) p2 ts sc
  | k `isKey` 'i' = GameState p1 (changeDirection p2 Game.Up) ts sc
  | k `isKey` 'j' = GameState p1 (changeDirection p2 Game.Left) ts sc
  | k `isKey` 'k' = GameState p1 (changeDirection p2 Game.Down) ts sc
  | k `isKey` 'l' = GameState p1 (changeDirection p2 Game.Right) ts sc
    where isKey k i = k == i || k == (toUpper i)
readEvent _ gs = gs
