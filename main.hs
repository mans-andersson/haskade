import UI.NCurses
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class(liftIO)
import Game

main = runCurses $ do
  w <- newWindow (rows+1) (columns+1) 0 0
  gameLoop w initialGameState

gameLoop :: Window -> GameState -> Curses ()
gameLoop w gs@(p1,p2,ts) = do
  updateWindow w $ do
    sequence drawWalls
    sequence (drawPlayer p1)
    sequence (drawPlayer p2)
  render
  t <- liftIO getPOSIXTime
  e <- getEvent w (Just 1)
  gameLoop w (moveGame gs (getMilliSeconds t))

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

getMilliSeconds :: POSIXTime -> Integer
getMilliSeconds t = round $ t * 1000
