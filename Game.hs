{-Written by Måns Andersson-}
module Game
       (
       -- Datatypes
       Direction (..),
       State (..),
       PBlock (..),
       GameState (..),
       -- Synonyms,
       Timestamp,
       Player,
       Score,
       -- Constants,
       rows,
       columns,
       centerX,
       centerY,
       timelimit,
       winningScore,
       initialP1,
       initialP2,
       initialGameState,
       -- Functions,
       getDirectionChar,
       changeDirection,
       moveGame,
       changeState
       ) where

data Direction = Left | Right | Up | Down deriving Eq

{-The State is a part of the GameState.-}
data State = Running | Player1Coll | Player2Coll | DoubleColl | Player1Won
  | Player2Won | Draw | Quit | MainMenu deriving Eq

data PBlock = PBlock {getDir :: Direction,
                      xCoord :: Integer,
                      yCoord :: Integer}

data GameState = GameState {getState :: State,
                            getP1 :: Player,
                            getP2 :: Player,
                            getTs :: Timestamp,
                            getScore :: Score}

type Timestamp = Integer
type Player = [PBlock] -- A player is a list of playerblocks
type Score = (Int, Int) -- (Player1, Player2)

rows = 50
columns = 50
centerY = rows `div` 2
centerX = columns `div` 2
{-timelimit specifies how often the game will be moved forward. That is to say
the players grow with one block. The number is defined in milliseconds.-}
timelimit = 100
winningScore = 5 :: Int

initialP1 = [PBlock Up 20 20, PBlock Up 20 21, PBlock Up 20 22]
initialP2 = [PBlock Up 30 20, PBlock Up 30 21, PBlock Up 30 22]
initialGameState = GameState MainMenu initialP1 initialP2 0 (0,0)

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
movePlayer (h:s:t) = newBlock ++ s:t
  where dir = getDir h
        newBlock = nextBlock h s

movePlayers :: GameState -> GameState
movePlayers (GameState s p1 p2 ts sc) =
  GameState s (movePlayer p1) (movePlayer p2) ts sc

{-Moves the game forward if the time since the last update has surpassed
a certain value in milliseconds. This controls the pace of the game.-}
moveGame :: Timestamp -> GameState -> GameState
moveGame time gs
  | (time - (getTs gs)) > timelimit = checkCollision . movePlayers $ updateTime gs time
  | otherwise = gs

{-Simply creates a new GameState that is exactly the same but with
  a new timestamp.-}
updateTime :: GameState -> Timestamp -> GameState
updateTime (GameState s p1 p2 oldt sc) newt = GameState s p1 p2 newt sc

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

{-Simply checks if two blocks have the same coordinates.-}
blockCollision :: PBlock -> PBlock -> Bool
blockCollision (PBlock _ x1 y1) (PBlock _ x2 y2) = x1 == x2 && y1 == y2

{-Has the player in the left argument collided with the player in the right
  argument.-}
playerCollision :: Player -> Player -> Bool
playerCollision (h:_) (_:t) = any (blockCollision h) t

{-Returns true if players collided with each other head to head.-}
chickenRaceCollision :: Player -> Player -> Bool
chickenRaceCollision (h1:_) (h2:_) = blockCollision h1 h2

{-Returns true if a player has collided with a wall.-}
wallCollision :: Player -> Bool
wallCollision (h:_)
  | x >= (columns-1) || x <= 0 = True
  | y >= (rows-1) || y <= 0 = True
  | otherwise = False
    where x = xCoord h
          y = yCoord h

{-Checks all the possible collisions and acts accordingly.-}
checkCollision :: GameState -> GameState
checkCollision gs
  | headcoll || (p1wall && p2wall) = changeState DoubleColl (incrementScore 1 . incrementScore 2 $ gs)
  | p1coll || p1wall = changeState Player1Coll (incrementScore 2 gs)
  | p2coll || p2wall = changeState Player2Coll (incrementScore 1 gs)
  | otherwise = gs
    where headcoll = chickenRaceCollision p1 p2
          p1coll = playerCollision p1 p2 || playerCollision p1 p1
          p2coll = playerCollision p2 p1 || playerCollision p2 p2
          p1wall = wallCollision p1
          p2wall = wallCollision p2
          p1 = getP1 gs
          p2 = getP2 gs

{-Simply creates a new GameState that is exactly like the old one but
  with a different inner state.-}
changeState :: State -> GameState -> GameState
changeState newS (GameState oldS p1 p2 ts sc) = GameState newS p1 p2 ts sc

{-Increments the score of a specified player.-}
incrementScore :: Integer -> GameState -> GameState
incrementScore n (GameState s p1 p2 ts (sc1,sc2))
  | n == 1 = GameState s p1 p2 ts (sc1+1,sc2)
  | n == 2 = GameState s p1 p2 ts (sc1,sc2+1)
  | otherwise = error "There only exists 2 players!"
