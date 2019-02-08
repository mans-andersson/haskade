import UI.NCurses
import Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import Control.Monad.IO.Class(liftIO)
import Data.Char(toUpper)
import Game

main = runCurses $ do
  setEcho False
  w <- newWindow (rows+1) (columns+1) 0 0
  gameLoop w initialGameState

{-The main gameloop!-}
gameLoop :: Window -> GameState -> Curses ()
gameLoop w gs@(p1,p2,ts) = do
  {-This part renders everything.-}
  updateWindow w $ do
    sequence drawWalls
    sequence (drawPlayer p1)
    sequence (drawPlayer p2)
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

getMilliSeconds :: POSIXTime -> Timestamp
getMilliSeconds t = round $ t * 1000

{-Checks if the event is a relevant button press and performs the relevant
  action.-}
readEvent :: Maybe Event -> GameState -> GameState
readEvent (Just (EventCharacter k)) gs@(p1,p2,ts)
  | k `isKey` 'w' = ((changeDirection p1 Game.Up),p2,ts)
  | k `isKey` 'a' = ((changeDirection p1 Game.Left),p2,ts)
  | k `isKey` 's' = ((changeDirection p1 Game.Down),p2,ts)
  | k `isKey` 'd' = ((changeDirection p1 Game.Right),p2,ts)
  | k `isKey` 'i' = (p1,(changeDirection p2 Game.Up),ts)
  | k `isKey` 'j' = (p1,(changeDirection p2 Game.Left),ts)
  | k `isKey` 'k' = (p1,(changeDirection p2 Game.Down),ts)
  | k `isKey` 'l' = (p1,(changeDirection p2 Game.Right),ts)
    where isKey k i = k == i || k == (toUpper i)
readEvent _ gs = gs
