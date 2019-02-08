module Game where

data Direction = Left | Right | Up | Down deriving Eq
data PBlock = PBlock {getDir :: Direction, xCoord :: Integer, yCoord :: Integer}
type Timestamp = Integer
type GameState = (Player, Player, Timestamp)
type Player = [PBlock]

rows = 60
columns = 60
{-timelimit specifies how often the game will be moved forward. That is to say
the players grow with one block. The number is defined in milliseconds.-}
timelimit = 400

initialGameState = ([PBlock Up 20 20, PBlock Up 20 21], [PBlock Up 30 20, PBlock Up 30 21], 0)

getDirectionChar :: Direction -> Char
getDirectionChar Game.Left = '←'
getDirectionChar Game.Right = '→'
getDirectionChar Game.Up = '↑'
getDirectionChar Game.Down = '↓'

{-Changes the direction of the player by creating a new Player list where
  the head of the list has had its direction changed to the direction specified
  in the argument.-}
changeDirection :: Player -> Direction -> Player
changeDirection (h:t) d = (PBlock d currentX currentY):t
  where currentX = xCoord h
        currentY = yCoord h

{-Moves the player by creating a new Player list with an added PBlock.
  The position of the new PBlock is determined by the direction of the
  last one.-}
movePlayer :: Player -> Player
movePlayer (h:s:t) = newBlock ++ t
  where dir = getDir h
        newBlock = nextBlock h s

movePlayers :: GameState -> GameState
movePlayers (p1,p2,ts) = (movePlayer p1, movePlayer p2,ts)

{-Moves the game forward if the time since the last update has surpassed
a certain value in milliseconds. This controls the pace of the game.-}
moveGame :: Timestamp -> GameState -> GameState
moveGame time gs@(p1,p2,ts)
  | (time - ts) > 400 = movePlayers (p1,p2,time)
  | otherwise = gs

{-Creates a new block to make the player longer. The most recent block may
need to be replaced if the player tries to move straight back into
themselves.-}
nextBlock :: PBlock -> PBlock -> [PBlock]
nextBlock b1@(PBlock d1 x1 y1) b2@(PBlock d2 x2 y2)
  | (d1, d2) == (Game.Left, Game.Right) = [PBlock d2 (x1+1) y1, PBlock d2 x1 y1]
  | (d1, d2) == (Game.Right, Game.Left) = [PBlock d2 (x1-1) y1, PBlock d2 x1 y1]
  | (d1, d2) == (Game.Up, Game.Down) = [PBlock d2 x1 (y1+1), PBlock d2 x1 y1]
  | (d1, d2) == (Game.Down, Game.Up) = [PBlock d2 x1 (y1-1), PBlock d2 x1 y1]
  | d1 == Game.Left = [PBlock d1 (x1-1) y1, b1]
  | d1 == Game.Right = [PBlock d1 (x1+1) y1, b1]
  | d1 == Game.Up = [PBlock d1 x1 (y1-1), b1]
  | d1 == Game.Down = [PBlock d1 x1 (y1+1), b1]
