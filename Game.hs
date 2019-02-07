module Game where

data Direction = Left | Right | Up | Down deriving Eq
data PBlock = PBlock {getDir :: Direction, xCoord :: Integer, yCoord :: Integer}
type Timestamp = Integer
type GameState = (Player, Player, Timestamp)
type Player = [PBlock]

rows = 60
columns = 60

initialGameState = ([PBlock Up 20 20, PBlock Up 20 21], [PBlock Up 30 20, PBlock Up 30 21], 0)

getDirectionChar :: Direction -> Char
getDirectionChar Game.Left = '←'
getDirectionChar Game.Right = '→'
getDirectionChar Game.Up = '↑'
getDirectionChar Game.Down = '↓'

changeDirection :: Player -> Direction -> Player
changeDirection (h:t) d = (PBlock d currentX currentY):t
  where currentX = xCoord h
        currentY = yCoord h

movePlayer :: Player -> Player
movePlayer p@(h:s:_) = newBlock:p
  where dir = getDir h
        newBlock = nextBlock h s

movePlayers :: GameState -> GameState
movePlayers (p1,p2,ts) = (movePlayer p1, movePlayer p2,ts)

{-Moves the game forward if the time since the last update has surpassed a certain value in milliseconds.
This controls the pace of the game.-}
moveGame :: GameState -> Integer-> GameState
moveGame gs@(p1,p2,ts) time
  | (time - ts) > 400 = movePlayers (p1,p2,time)
  | otherwise = gs

nextBlock :: PBlock -> PBlock -> PBlock
nextBlock (PBlock d1 x1 y1) (PBlock d2 x2 y2)
  | (d1, d2) == (Game.Left, Game.Right) = PBlock d2 (x1+1) y1
  | (d1, d2) == (Game.Right, Game.Left) = PBlock d2 (x1-1) y1
  | (d1, d2) == (Game.Up, Game.Down) = PBlock d2 x1 (y1+1)
  | (d1, d2) == (Game.Down, Game.Up) = PBlock d2 x1 (y1-1)
  | d1 == Game.Left = PBlock d1 (x1-1) y1
  | d1 == Game.Right = PBlock d1 (x1+1) y1
  | d1 == Game.Up = PBlock d1 x1 (y1-1)
  | d1 == Game.Down = PBlock d1 x1 (y1+1)
