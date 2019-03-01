import System.Random
import Data.List
import Debug.Trace

type Position = (Int, Int)
type Mine = Bool
type NeighbourMines = Int

data State = Revealed
           | Hidden deriving (Show, Read, Eq)

data Square = Square Position State Mine NeighbourMines deriving (Show, Read, Eq)

type Board = [Square]


{- mkBoard n
   Initialize the board using list comprehension,
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
randomCoords :: Int -> IO [Position]
randomCoords 0 = return []
randomCoords n = do
    x <- randomRIO (1, 18)
    y <- randomRIO (1, 18)

    rs <- randomCoords (n-1)

    return ((x, y) : rs)


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
printTop n = ['A'..'Z']!!(n-1) : " " ++ printTop (n-1)

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

getMines :: Int -> IO [Position]
getMines n = do
  mines <- randomCoords n

  if (dupMines mines) then getMines n
    else return mines



countMines :: Board -> Int
countMines [] = 0
countMines (square@(Square _ _ True _):s) = 1 + countMines s
countMines (square:s) = countMines s



getNeighbourSquares :: Position -> Board -> Board
getNeighbourSquares (x,y) board
  | x == 1  && y == 1  = [(getSquare (x,y+1) board),
                          (getSquare (x+1, y+1) board),
                          (getSquare (x+1, y) board)
                         ]
  | x == 1  && y == 18 = [(getSquare (x, y-1) board),
                          (getSquare (x+1, y-1) board),
                          (getSquare (x+1, y) board)
                         ]
  | y == 1  && x == 18 = [(getSquare (x-1, y) board),
                          (getSquare (x-1, y+1) board),
                          (getSquare (x, y+1) board)
                         ]
  | y == 18 && x == 18 = [(getSquare (x, y-1) board),
                          (getSquare (x-1, y-1) board),
                          (getSquare (x-1, y) board)
                         ]
  | x == 1             = [(getSquare (x,y-1) board),
                          (getSquare (x,y+1) board),
                          (getSquare (x+1,y-1) board),
                          (getSquare (x+1,y) board),
                          (getSquare (x+1,y+1) board)
                         ]
  | y == 1             = [(getSquare (x-1,y) board),
                          (getSquare (x-1,y+1) board),
                          (getSquare (x,y+1) board),
                          (getSquare (x+1,y) board),
                          (getSquare (x+1,y+1) board)
                          ]
  | x == 18            = [(getSquare (x-1,y-1) board),
                         (getSquare (x-1,y) board),
                         (getSquare (x-1,y+1) board),
                         (getSquare (x,y-1) board),
                         (getSquare (x,y+1) board) 
                         ]
  | y == 18            = [(getSquare (x-1,y-1) board),
                          (getSquare (x-1,y) board),
                          (getSquare (x,y-1) board),
                          (getSquare (x+1,y-1) board),
                          (getSquare (x+1,y) board)
                         ]
  | otherwise =   [ (getSquare (x-1,y-1) board),
                    (getSquare (x-1,y) board),
                    (getSquare (x-1,y+1) board),
                    (getSquare (x,y-1) board),
                    (getSquare (x,y+1) board),
                    (getSquare (x+1,y-1) board),
                    (getSquare (x+1,y) board),
                    (getSquare (x+1,y+1) board)
                    ]

getSquare :: Position -> Board -> Square
getSquare pos [] = (Square pos Hidden False 0)
getSquare pos (square@(Square spos _ _ _):s)
  | pos == spos = square
  | otherwise = getSquare pos s


countNeighbourMines :: Board -> Int
countNeighbourMines [] = 0
countNeighbourMines (neighbour:s) = (countMine neighbour) + countNeighbourMines s

countMine :: Square -> Int
countMine (Square _ _ True _) = 1
countMine _ = 0


initNeighbours :: Board -> Board -> Board
initNeighbours [] boardcpy = []
initNeighbours board@(square@(Square pos state mine neighbours):s) boardcpy =
             (Square pos state mine (countNeighbourMines $ getNeighbourSquares pos boardcpy): initNeighbours s boardcpy)

toPosition :: String -> Position
toPosition s = read s :: Position


mkChoice :: Position -> Board -> Board
mkChoice _ [] = []
mkChoice (x,y) (square@(Square pos state mine mines):s)
  | (x,y) == pos && mines == 0 = openEmptyAdjacent((Square pos Revealed mine mines):s)
  | (x,y) == pos = (Square pos Revealed mine mines) : s
  | otherwise = square : mkChoice (x,y) s

openEmptyAdjacent :: Board -> Board
openEmptyAdjacent board@((Square (x,y) _ False adj):s) 
  | adj == 0 = trace ("(" ++ show x ++ "," ++ show y ++ ")") $ concat (map mkChoiceAux [(getNeighbourSquares (x,y) board)])
  | otherwise = board 

mkChoiceAux :: Board -> Board
mkChoiceAux ((Square (x,y) state mine mines):s) = mkChoice (x,y) ((Square (x,y) state mine mines):s)

play :: Board -> IO ()
play board = do

  prettyPrint $ printBoard board 18

  putStrLn "Make your move"

  choice <- getLine

  let newBoard = mkChoice (toPosition choice) board

  play newBoard




{- main
   Just testing stuff
-}
main :: IO ()
main = do
    putStrLn "Welcome to Minesweeper v.000000000001"

    let board = mkBoard 18

    mines <- getMines 1

    let newBoard = insertMines mines board
    let playBoard = initNeighbours newBoard newBoard

    play playBoard
