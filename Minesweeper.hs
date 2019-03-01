import System.Random
import Data.List
import Data.Char
import Control.Monad

type Position = (Int, Int)
type Mine = Bool
type NeighbourMines = Int

data State = Revealed 
           | Hidden deriving (Show, Read)

data Square = Square Position State Mine NeighbourMines deriving (Show, Read)

type Board = [Square]



{- mkBoard n
   Initilize the board using list comprehension, 
   this makes squares with a default value for every position set to the maximum by the input
PRE:
RETURNS: A minesweeper board as a list.
SIDE EFFECTS:
EXAMPLES: mkBoard 0 = []
          mkBoard 1 = [Square (1,1) Revealed False 0]
          mkBoard 3 = [Square (1,1) Revealed False 0,Square (1,2) Revealed False 0,Square (1,3) Revealed False 0,Squ          are (2,1) Revealed False 0,Square (2,2) Revealed False 0,Square (2,3) Revealed False 0,Square (3,1) Reveal          ed False 0,Square (3,2) Revealed False 0,Square (3,3) Revealed False 0]
-}
mkBoard :: Int -> Board
mkBoard n = [(Square (x,y) Hidden False 0) 
            | x <- [1..n], y <- [1..n]]

{- insertMines mines board
   insert a list of mines into the board. The mines are represented as bools.
PRE:
RETURNS: A list with mines inserted to the borad at positions based on the given coordinates.
SIDE EFFECTS:
VARIANT: Length of "mines".
EXAMPLES: insertMines [] [] = []
          insertMines [] (mkBoard 0) = []
          insertMines [(1,1)] (mkBoard 0) = []
          insertMines [(1,1), (2,3)] (mkBoard 3) = [Square (1,1) Revealed True 1,Square (1,2) Revealed False 0,Squar          e (1,3) Revealed False 0,Square (2,1) Revealed False 0,Square (2,2) Revealed False 0,Square (2,3) Revealed          True 1,Square (3,1) Revealed False 0,Square (3,2) Revealed False 0,Square (3,3) Revealed False 0]
-}
insertMines :: [Position] -> Board -> Board
insertMines [] board     = board
insertMines (p:ps) []    = []
insertMines (p:ps) board = insertMines ps (insertMine p board)

insertMine :: Position -> Board -> Board
insertMine _ [] = []
insertMine position (square@(Square pos _ _ _):squares)
  | position == pos = (Square pos Hidden True 1) : squares
  | otherwise = square : insertMine position squares


{- randomCoords n
   Creates a list of n random coordinates.
PRE:
RETURNS: A list of n random positions represented as tuples.
SIDE EFFECTS:
EXAMPLES: randomCoords 0 = []
          randomCoords 2 = [(13,3),(2,9)]
-}

randomCoord :: Int -> IO Position
randomCoord n = do
    x <- randomRIO (1, n)
    y <- randomRIO (1, n)

    return (x,y)

{- prettyPrint n
   Outputs the string on the terminal
-}
prettyPrint :: String -> IO ()
prettyPrint n = putStrLn n


printBoard :: Board -> Int -> String
printBoard []     n = []
printBoard (x@(Square (x1,y1) _ _ _):xs) n
  | y1 == 1 && x1 == 1 = "   " ++ (reverse $ printTop n) ++ "\n" ++ (show x1) ++ " | " ++ printSquare x ++ " " ++ printBoard xs n
  | y1 == 1 = if x1 < 10 then (show x1) ++ " | " ++ printSquare x ++ " " ++ printBoard xs n 
    else (show x1) ++ "| " ++ printSquare x ++ " " ++ printBoard xs n
  | y1 == n = printSquare x ++ "\n" ++ printBoard xs n
  | otherwise = printSquare x ++ " " ++ printBoard xs n

printTop :: Int -> String
printTop 0 = []
printTop n = ['a'..'z']!!(n-1) : " " ++ printTop (n-1)

{- printSquare square
   Convert a square into an ASCII-character
-}
printSquare :: Square -> String
printSquare (Square _ Hidden _ _) = "#"
printSquare (Square _ Revealed True n) = "*"
printSquare (Square _ Revealed _ 0) = " "
printSquare (Square _ Revealed _ n) = show n


dupMines :: [Position] -> Bool
dupMines mines = if length remDups < length mines then True else False
  where remDups = map head $ group $ sort mines

getMines :: Int -> Int -> [Position] -> IO [Position]
getMines 0 size mines = return mines
getMines n size mines = do
  mine <- randomCoord size
  let mineList = (mine:mines)

  if (dupMines mineList) then getMines n size mines
    else getMines (n-1) size mineList


countMines :: Board -> Int
countMines [] = 0 
countMines (square@(Square _ _ True _):s) = 1 + countMines s
countMines (square:s) = countMines s

{- getNeighbourSquares
   get all surrounding squares of hte given position
-}
getNeighbourSquares :: Position -> Board -> Board
getNeighbourSquares (x,y) board = [
                    (getSquare (x-1,y-1) board),
                    (getSquare (x-1,y) board),
                    (getSquare (x-1,y+1) board),
                    (getSquare (x,y-1) board),
                    (getSquare (x,y+1) board),
                    (getSquare (x+1,y-1) board),
                    (getSquare (x+1,y) board),
                    (getSquare (x+1,y+1) board)
                    ]

{- getSquare position board
   get the corresponding square from the given position
-}
getSquare :: Position -> Board -> Square
getSquare pos [] = (Square pos Hidden False 0)
getSquare pos (square@(Square spos _ _ _):s)
  | pos == spos = square
  | otherwise = getSquare pos s

{- countNeighbourMines neighbours
   counts all mines of neighbours
-}
countNeighbourMines :: Board -> Int
countNeighbourMines [] = 0
countNeighbourMines (neighbour:s) = (countMine neighbour) + countNeighbourMines s

-- If square has a mine, this will output 1 else 0
countMine :: Square -> Int
countMine (Square _ _ True _) = 1
countMine _ = 0

{- initNeighbours board boardcpy
   This function outputs a new board with NeighbourMines initialized for all squares.
-}
initNeighbours :: Board -> Board -> Board
initNeighbours [] boardcpy = []
initNeighbours board@(square@(Square pos state mine neighbours):s) boardcpy =
             (Square pos state mine (countNeighbourMines $ getNeighbourSquares pos boardcpy): initNeighbours s boardcpy)

{- mkChoice position board
   This function takes a position and reveal the state of that square.
-}
mkChoice :: Maybe Position -> Board -> Board
mkChoice Nothing board = board
mkChoice _ [] = []
mkChoice (Just (x,y)) (square@(Square pos state mine mines):s)
  | (x,y) == pos = (Square pos Revealed mine mines) : s
  | otherwise = square : mkChoice (Just (x,y)) s

{- choiceMine position board
   Checks if given position has a mine on it, if it does, it returns true else false.
-}
choiceMine :: Maybe Position -> Board -> Bool
choiceMine _ [] = False
choiceMine Nothing board = False
choiceMine (Just (x,y)) (square@(Square pos state mine mines):s)
  | (x,y) == pos && mine = mine
  | otherwise = choiceMine (Just (x,y)) s

{- toPosition s
   This function translates a given move (ex. "1f") into its position (ex. (1,6))
-}
toPosition :: String -> Maybe Position
toPosition (i1:i2:c:[]) = if isDigit i1 && isDigit i2 && isAlpha c then Just (read (i1 : i2 : []) :: Int, chartoInt c cilist) else Nothing
toPosition (i:c:[]) = if isDigit i && isAlpha c then Just (digitToInt i, chartoInt c cilist) else Nothing
toPosition _ = Nothing

{- chartoInt
   Outputs the number that is mapped to the given character.
-}
chartoInt :: Char -> [(Char,Int)] -> Int
chartoInt c (x:xs)
  | c == fst x = snd x
  | otherwise = chartoInt c xs

-- A list of tuples where characters are mappe to the corresponding numbers.
cilist = zip ['a'..'z'] [1..26]

{- play board size
   In this function we handle all the moves from the user,
   check if the game is over and prints the board. 
-}
play :: Board -> Int -> IO ()
play board size = do

  putStrLn "Make your move"

  choice <- getLine

  let newBoard = mkChoice (toPosition choice) board

  prettyPrint $ printBoard newBoard size

  if choiceMine (toPosition choice) board then putStrLn "You Lost!" else play newBoard size

{- mkDifficulty s
   Outputs the size of the board and amount of mines depending on the desired difficulty
-}
mkDifficulty :: String -> (Int, Int)
mkDifficulty "easy" = (3,3)
mkDifficulty "medium" = (8,8)
mkDifficulty "hard" = (18,18)
mkDifficulty "custom" = undefined

{- main
   Just testing stuff
-}
main :: IO ()
main = do
    putStrLn "Welcome to Minesweeper v.000000000001"

    putStrLn "Please enter a difficulty (easy/medium/hard) or choose custom"

    getDifficulty <- getLine

    let difficulty = mkDifficulty getDifficulty

    let board = mkBoard (fst difficulty)

    mines <- getMines (snd difficulty) (fst difficulty) []

    let newBoard = insertMines mines board
    let playBoard = initNeighbours newBoard newBoard

    prettyPrint $ printBoard playBoard (fst difficulty)

    play playBoard (fst difficulty)