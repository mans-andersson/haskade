module Game where

data Direction = Left | Right | Up | Down deriving Eq
data PBlock = PBlock {getDir :: Direction, xCoord :: Integer, yCoord :: Integer}
type GameState = (Player, Player)
type Player = [PBlock]

rows = 50
columns = 100

initialGameState = ([PBlock Up 20 20, PBlock Up 20 21], [PBlock Up 70 20, PBlock Up 70 21])

getDirectionChar :: Direction -> Char
getDirectionChar Game.Left = '←'
getDirectionChar Game.Right = '→'
getDirectionChar Game.Up = '↑'
getDirectionChar Game.Down = '↓'

movePlayer :: Player -> Player
movePlayer p@(h:s:_) = newBlock:p
  where dir = getDir h
        newBlock = nextBlock h s

movePlayers :: GameState -> GameState
movePlayers (p1,p2) = (movePlayer p1, movePlayer p2)

{-Moves the game forward if the provided millisecond value has as certain
result in a modolo computation. This controls the pace of the game.-}
moveGame :: GameState -> Integer-> GameState
moveGame gs time
  | time `mod` 500 == 0 = movePlayers gs
  | otherwise = gs

nextBlock :: PBlock -> PBlock -> PBlock
nextBlock (PBlock d1 x1 y1) (PBlock d2 x2 y2)
  | (d1, d2) == (Game.Left, Game.Right) = PBlock d1 (x1+1) y1
  | (d1, d2) == (Game.Right, Game.Left) = PBlock d1 (x1-1) y1
  | (d1, d2) == (Game.Up, Game.Down) = PBlock d1 x1 (y1+1)
  | (d1, d2) == (Game.Down, Game.Up) = PBlock d1 x1 (y1-1)
  | d1 == Game.Left = PBlock d1 (x1-1) y1
  | d1 == Game.Right = PBlock d1 (x1+1) y1
  | d1 == Game.Up = PBlock d1 x1 (y1-1)
  | d1 == Game.Down = PBlock d1 x1 (y1+1)
