import System.Random

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
-}
mkBoard :: Int -> Board
mkBoard n = [(Square (x,y) Revealed False 1) 
            | x <- [1..n], y <- [1..n]]

{- insertSquare mines board
   First of all, this function has a big problem. It doesn't insert more than one mine into the board.
   It needs to save the board with the inserted mine and the call the function again with that board and the new mine.
   I haven't figured out how to implement that bit yet

   This function takes a list of squares that has mines and the existing board.
   It matches the position of hte mine and insert that into the board, otherwise it traverses through the board.
-}
insertMines :: [Position] -> Board -> Board
insertMines [] board     = board
insertMines (p:ps) board = insertMines ps (insertMine p board)

insertMine :: Position -> Board -> Board
insertMine position (square@(Square pos _ _ _):squares)
  | position == pos = (Square pos Revealed True 1) : squares
  | otherwise = square : insertMine position squares


{- randomBoard n
   A clusterfuck of functions lol, just trying to make a board that includes mines at random positions.
   The input is how many mines we will insert into the board, 
   we get n positions in a list and send those to the function 'insertSquare', that does what it says.

   This will get a board from 'insertSquare' and the returns it.
-}


randomCoords :: Int -> IO [Position]
randomCoords 0 = return []
randomCoords n = do
    x <- randomRIO (1, 6)
    y <- randomRIO (1, 6)

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
  | y1 == n = printSquare x ++ "\n" ++ printBoard xs n
  | otherwise = printSquare x ++ printBoard xs n


{- printSquare square
   Convert a square into an ASCII-character
-}
printSquare :: Square -> String
printSquare (Square _ Hidden _ _) = "#"
printSquare (Square _ Revealed _ 0) = " "
printSquare (Square _ Revealed True n) = "*"
printSquare (Square _ Revealed _ n) = show n


{- main
   Just testing stuff
-}
main :: IO ()
main = do
    putStrLn "Welcome to Minesweeper v.000000000001"

    let board = mkBoard 6

    mines <- randomCoords 6

    let newBoard = insertMines mines board

    prettyPrint $ printBoard newBoard 6

