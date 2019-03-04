
import System.Random
import Data.List
import Data.Char
import Control.Monad
import Data.Maybe
import Text.Read
import Debug.Trace

{-
<<<<<<< HEAD
Position represents a position on the board as a pari of coordinates represented as a tuple.
=======
this datatype represents a position on the board as a pari of coordinates represented as a tuple.
HOW
INVARIANT: 
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}
type Position = (Int, Int)

{-
<<<<<<< HEAD
Mine represents the mines on the board as bools when a square is opened.
=======
this datatype represents the mines on the board as bools when a square is opened.
HOW
INVARIANT:
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

type Mine = Bool

{-
NeighbourMines represent the integers on the board, which indicates how many mines there are adjacent to the current square.
<<<<<<< HEAD
=======
HOW:
INVARIANT:
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

type NeighbourMines = Int


<<<<<<< HEAD
{- 
Flag represent the
=======
{- Flag
HOW:
INVARIANT:
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

type Flag = Bool

{-
The datatype Board represent the game field as a list of Positions that creates a square.
<<<<<<< HEAD
-}
type Board = [Square]

{-

-}
=======
INVARIANT n Stuff !
-}
type Board = [Square]

>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
data State = Revealed
           | Hidden deriving (Show, Eq)

{-
Square represents stuff -} --PERHAPS MOVE THIS ONE ABOVE BOARD?
data Square = Square Position State Mine NeighbourMines Flag deriving (Show,Eq)


{- mkBoard n
Initilize the board using list comprehension, 
This makes squares with a default value for every position set to the maximum by the input.
PRE: True.
RETURNS: A minesweeper board as a list.
SIDE EFFECTS:
EXAMPLES: mkBoard 0 = []
<<<<<<< HEAD
          mkBoard 1 = [Square (1,1) Hidden False 0 False]
          mkBoard 3 = [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False,Square (2,3) Revealed False 0 False, Square (3,1) Revealed False 0 False,Square (3,2) Revealed False 0 False,Square (3,3) Revealed False 0 False]
-}

mkBoard :: Int -> Board
=======
          mkBoard 1 = [Square (1,1) Revealed False 0]
          mkBoard 3 = [Square (1,1) Revealed False 0,Square (1,2) Revealed False 0,Square (1,3) Revealed False 0,Squ          are (2,1) Revealed False 0,Square (2,2) Revealed False 0,Square (2,3) Revealed False 0,Square (3,1) Reveal          ed False 0,Square (3,2) Revealed False 0,Square (3,3) Revealed False 0]
-}

mkBoard :: Int -> Board

>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
mkBoard n = [(Square (x,y) Hidden False 0 False) 
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
<<<<<<< HEAD
          insertMines [(1,1), (2,3)] (mkBoard 3) = [Square (1,1) Revealed True 1 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0,Square (2,2) Revealed False 0,Square (2,3) Revealed          True 1,Square (3,1) Revealed False 0,Square (3,2) Revealed False 0,Square (3,3) Revealed False 0]
=======
          insertMines [(1,1), (2,3)] (mkBoard 3) = [Square (1,1) Revealed True 1,Square (1,2) Revealed False 0,Squar          e (1,3) Revealed False 0,Square (2,1) Revealed False 0,Square (2,2) Revealed False 0,Square (2,3) Revealed          True 1,Square (3,1) Revealed False 0,Square (3,2) Revealed False 0,Square (3,3) Revealed False 0]
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

insertMines :: [Position] -> Board -> Board
insertMines [] board     = board
insertMines (p:ps) []    = []
insertMines (p:ps) board = insertMines ps (insertMine p board)


{- insertMine position (square@(Square pos _ _ _):squares)
inserts a mine at the given position. 
PRE: Position has to exist.
RETURNS: A list of squares, containing one mine from position.
SIDE EFFECTS:
VARIANT: ?
EXAMPLES: insertMine (1,6) [] = []
<<<<<<< HEAD
          insertMine (1,2) (mkBoard 2) =  [Square (1,1) Hidden False 0 False,Square (1,2) Hidden True 1 False, Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False]         
=======
          insertMine (1,2) (mkBoard 2) =  [Square (1,1) Hidden False 0,Square (1,2) Hidden True 1,Square (2,1) Hidden False 0,Square (2,2) Hidden False 0]         
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
 -} 

insertMine :: Position -> Board -> Board
insertMine _ [] = []
insertMine position (square@(Square pos _ _ _ _):squares)
  | position == pos = (Square pos Hidden True 1 False) : squares
  | otherwise = square : insertMine position squares


{- randomCoords n
Creates a random value of the type Position where the two values can not be larger than n
PRE: True
RETURNS: A tuple with two random integers as first and second values.
SIDE EFFECTS:
EXAMPLES: randomCoords 0 = (1,1)
          randomCoords 2 = (1,2)
-}

randomCoord :: Int -> IO Position
randomCoord n = do

    x <- randomRIO (1, n)
    y <- randomRIO (1, n)

    return (x,y)


{- prettyPrint n
Outputs the string on the terminal.
PRE: True.
RETURNS: A string without ' "" '.
SIDE EFFECTS:
EXAMPLES: prettyPrint "" =
          prettyPrint "Hello World!" = Hello World!
          prettyPrint "I love numbers: 123234" = I love numbers: 123234  
-}


prettyPrint :: String -> IO ()
prettyPrint n = putStrLn n


<<<<<<< HEAD
{- printBoard (x@(Square (x1,y1) _ _ _ _):xs) n
=======
{- printBoard (x@(Square (x1,y1) _ _ _):xs) n
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
PRE:
RETURNS: A string representing a game board.
SIDE EFFECTS:
EXAMPLES: printBoard []  5  = ""
          printBoard (mkBoard 2) 5 = "    a b c d e\n1 | # # 2 | # # "
          
-}


printBoard :: Board -> Int -> String
printBoard []  n  = []
printBoard (x@(Square (x1,y1) _ _ _ _):xs) n
  | y1 == 1 && x1 == 1 = "   " ++ (reverse $ printTop n) ++ "\n" ++ (show x1) ++ " | " ++ printSquare x ++ " " ++ printBoard xs n
  | y1 == 1 = if x1 < 10 then (show x1) ++ " | " ++ printSquare x ++ " " ++ printBoard xs n 
    else (show x1) ++ "| " ++ printSquare x ++ " " ++ printBoard xs n
  | y1 == n = printSquare x ++ "\n" ++ printBoard xs n
  | otherwise = printSquare x ++ " " ++ printBoard xs n

{- printTop n 
Prints the alphabet in reversed order with the first n letters.
PRE: True.
RETURNS: A string of the letters in the alphabet in reversed order, with upper case letters 
 when n is larger than the amount of letters in the alphabet.
SIDE EFFECTS:
EXAMPLES: printTop 0 = ""
          printTop 26 = "z y x w v u t s r q p o n m l k j i h g f e d c b a "
          printTop 52 = "Z Y X W V U T S R Q P O N M L K J I H G F E D C B A z y x w v u t s r q p o n m l k j i h g f e d c b a "
          printTop 53 = "Sorry pal!" 
-}

printTop :: Int -> String
<<<<<<< HEAD
=======

>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
printTop 0 = []
printTop n | n < 53 = (['a'..'z'] ++ ['A'..'Z'])!!(n-1) : " " ++ printTop (n-1)
printTop n = "Sorry pal!"


{- printSquare square
Convert a square into an ASCII-character.
PRE:
RETURNS: An ASCII-character fulfilling the given condition.
SIDE EFFECTS:
<<<<<<< HEAD
EXAMPLES: printSquare (Square (1,1) Hidden False 0 False) = "#"
          printSquare (Square (2,2) Revealed False 7 False) = "7"
          printSquare (Square (2,2) Revealed False 7 True) = "F"
=======
EXAMPLES: printSquare (Square (1,1) Hidden False 0) = "#"
          printSquare (Square (2,2) Revealed False 7) = "7"
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}




printSquare :: Square -> String
printSquare (Square _ _ _ _ True) = "F"
printSquare (Square _ Hidden _ _ _) = "#"
printSquare (Square _ Revealed True n _) = "*"
printSquare (Square _ Revealed _ 0 _) = " "
printSquare (Square _ Revealed _ n _) = show n


{-dupMines mines
Checks if a position is occupied by a mine or not. || Checks if there are any duplicate tuples in a list.
PRE: True.
RETURNS: A bool that indicates whether there is a duplicate tuple or not.
SIDE EFFECTS:
EXAMPLES: dupMines [] = False
          dupMines [(1,2), (1,5)] = False
          dupMines [(1,5),(1,5)] = True
          dupMines [(1,5),(1,5), (1,3)] = True
-}


dupMines :: [Position] -> Bool
dupMines mines = length remDups < length mines
  where remDups = map head $ group $ sort mines


{- getMines n size mines
PRE:
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}


getMines :: Int -> Int -> [Position] -> IO [Position]
getMines 0 size mines = return mines
getMines n size mines = do

  mine <- randomCoord size

  let mineList = (mine:mines)

  if (dupMines mineList) then getMines n size mines

    else getMines (n-1) size mineList


{- countMines (square@(Square _ _ True _):s)
Counts the number of mines at the board.
PRE: True.
RETURNS: An integer representing the number of mines in the called list.
SIDE EFFECTS:
VARIANT: Length (square:s)
EXAMPLES: countMines [] = 0
<<<<<<< HEAD
          countMines [Square (1,1) Revealed True 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden True 0 False] = 2
          countMines [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden True 0 False] = 1
=======
          countMines [Square (1,1) Revealed True 0,Square (1,2) Hidden False 0,Square (2,1) Hidden False 0,Square (2,2) Hidden True 0] = 2
          countMines [Square (1,1) Hidden False 0,Square (1,2) Hidden False 0,Square (2,1) Hidden False 0,Square (2,2) Hidden True 0] = 1
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}


countMines :: Board -> Int
countMines [] = 0 
countMines (square@(Square _ _ True _ _):s) = 1 + countMines s
countMines (square:s) = countMines s


{- getNeighbourSquares
<<<<<<< HEAD
Get all surrounding squares of the given position.
PRE: (x,y) has to be a coordinate of the board and size == size of board
RETURNS: A list of the squares that surround the  square with position (x,y)
EXAMPLES: getNeighbourSquares (2,2) Board 3 = [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden False 0 False]
-}

=======
Get all surrounding squares of hte given position.
PRE:
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}



>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
getNeighbourSquares :: Position -> Board -> Int -> Board
getNeighbourSquares (x,y) board size
  | x == 1  && y == 1  = [(getSquare (x,y+1) board),
                          (getSquare (x+1, y+1) board),
                          (getSquare (x+1, y) board)
                         ]


  | x == 1  && y == size = [(getSquare (x, y-1) board),
                            (getSquare (x+1, y-1) board),
                            (getSquare (x+1, y) board)
                           ]


  | y == 1  && x == size = [(getSquare (x-1, y) board),
                            (getSquare (x-1, y+1) board),
                            (getSquare (x, y+1) board)
                           ]


  | y == size && x == size = [(getSquare (x, y-1) board),
                              (getSquare (x-1, y-1) board),
                              (getSquare (x-1, y) board)
                             ]


  | x == 1               = [(getSquare (x,y-1) board),
                            (getSquare (x,y+1) board),
                            (getSquare (x+1,y-1) board),
                            (getSquare (x+1,y) board),
                            (getSquare (x+1,y+1) board)
                           ]


  | y == 1               = [(getSquare (x-1,y) board),
                            (getSquare (x-1,y+1) board),
                            (getSquare (x,y+1) board),
                            (getSquare (x+1,y) board),
                            (getSquare (x+1,y+1) board)
                           ]


  | x == size            = [(getSquare (x-1,y-1) board),
                            (getSquare (x-1,y) board),
                            (getSquare (x-1,y+1) board),
                            (getSquare (x,y-1) board),
                            (getSquare (x,y+1) board) 
                           ]


  | y == size            = [(getSquare (x-1,y-1) board),
                            (getSquare (x-1,y) board),
                            (getSquare (x,y-1) board),
                            (getSquare (x+1,y-1) board),
                            (getSquare (x+1,y) board)
                           ]


  | otherwise =   [(getSquare (x-1,y-1) board),
                   (getSquare (x-1,y) board),
                   (getSquare (x-1,y+1) board),
                   (getSquare (x,y-1) board),
                   (getSquare (x,y+1) board),
                   (getSquare (x+1,y-1) board),
                   (getSquare (x+1,y) board),
                   (getSquare (x+1,y+1) board)
                  ]

{- getSquare position board
Get the corresponding square from the given position.
PRE:
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}

getSquare :: Position -> Board -> Square
getSquare pos (square@(Square spos _ _ _ _):s)
  | pos == spos = square
  | otherwise = getSquare pos s
<<<<<<< HEAD
getSquare pos board = (Square pos Hidden False 0 False)


{- countNeighbourMines (neighbour:s)
Counts all mines of neighbours.
PRE: (neighbour:s) is a list of the squares surrounding a square
VARIANT: length (neighbour:s)
RETURNS: The amount of mines present in (neighbour:s)
EXAMPLES: countNeighbourMines [Square (1,1) Hidden True 1 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden True 1 False] = 2
          countNeighbourMines [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden False 0 False] = 0
-}

=======


{- countNeighbourMines neighbours
Counts all mines of neighbours.
-}


>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
countNeighbourMines :: Board -> Int
countNeighbourMines [] = 0
countNeighbourMines (neighbour:s) = (countMine neighbour) + countNeighbourMines s

<<<<<<< HEAD
{-countMine square
Gives an int that indicates wether or not the square has a mine.
PRE: True
RETURNS: 1 if Square has a mine, 0 if square does not have a mine
EXAMPLES: countMine (Square (1,1) Hidden True 1 False) = 1
          countMine (Square (2,2) Hidden False 1 False) = 0
-}

=======
{-countMine (Square _ _ _ _
-}

-- If square has a mine, this will output 1 else 0
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
countMine :: Square -> Int
countMine (Square _ _ True _ _) = 1
countMine _ = 0


{- initNeighbours board boardcpy
   This function outputs a new board with NeighbourMines initialized for all squares.
-}

initNeighbours :: Board -> Board -> Int -> Board
initNeighbours [] boardcpy _ = []
initNeighbours board@(square@(Square pos state mine neighbours flag):s) boardcpy size =
             (Square pos state mine (countNeighbourMines $ getNeighbourSquares pos boardcpy size) flag : initNeighbours s boardcpy size)


<<<<<<< HEAD
{- mkChoice position board boarcpy size
   Reveals the given position, if the square of that position has 0 surrounding mines it will reveal the neighbours recursively.
   PRE: size == size of board
   VARIANT: length board 
   RETURNS: A board that has revealed all given positions
   EXAMPLES: mkChoice Nothing (mkBoard 2) (mkBoard 2) 2 = [Square (1,1) Hidden False 0 False, Square (1,2) Hidden False 0 False, Square (2,1) Hidden False 0 False, Square (2,2) Hidden False 0 False]
             mkChoice (Just (1,1)) (mkBoard 2) (mkBoard 2) 2 = [Square (1,1) Revealed False 0 False, Square (1,2) Hidden False 0 False, Square (2,1) Hidden False 0 False, Square (2,2) Hidden False 0 False]
-}

mkChoice :: Maybe Position -> Board -> Board -> Int -> Board
mkChoice Nothing board cpy _ = board
=======
{- mkChoice position board
   This function takes a position and reveal the state of that square.
-}

mkChoice :: Maybe Position -> Board -> Board -> Int -> Board
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
mkChoice _ [] cpy _ = cpy
mkChoice (Just (x,y)) (square@(Square pos state mine mines flag):xs) cpy size
  | (x,y) == pos && state == Hidden && mines == 0 = revealNeighbours (hiddenNeighbours $ getNeighbourSquares pos cpy size) (insertSquare (Square pos Revealed mine mines flag) cpy) size
  | (x,y) == pos && state == Hidden = insertSquare (Square pos Revealed mine mines flag) cpy
  | otherwise = mkChoice (Just (x,y)) xs cpy size


<<<<<<< HEAD
{- revealNeighbours (x:xs) board size
Reveals all squares next to a chosen square with 0 adjacent mines, which again applies to any of the adjacent squares that also have 0 adjacent mines
PRE: (x:xs) must be a list of hidden squares
VARIANT: length board
RETURNS: A game board with the proper squares revealed
EXAMPLES: revealNeighbours [] board 3 = board
          revealNeighbours [Square (1,2) Hidden False 0 False,Square (2,2) Hidden False 0 False,Square (2,1) Hidden False 0 False] [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 1 False,Square (2,3) Hidden False 1 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 1 False,Square (3,3) Hidden True 0 False] 3 = [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 1 False,Square (2,3) Revealed False 1 False,Square (3,1) Revealed False 0 False,Square (3,2) Revealed False 1 False,Square (3,3) Hidden True 0 False]
=======
{- revealNeighbours COORDINATES!?!?!?  board size
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

revealNeighbours :: [Square] -> Board -> Int -> Board
revealNeighbours [] board _ = board
revealNeighbours (x:xs) board size = revealNeighbours xs (revealNeighbours' x board size) size


<<<<<<< HEAD
{-revealNeighbours' 
Reveals all squares next to a chosen square with 0 adjacent mines, which again applies to any of the adjacent squares that also have 0 adjacent mines
PRE: Square is hidden
RETURNS: A board with mkChoice called on the square at position pos
EXAMPLES: revealNeighbours' (Square (1,1) Hidden False 0 False) [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 1 False,Square (2,3) Hidden False 1 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 1 False,Square (3,3) Hidden True 0 False] 3 = [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 1 False,Square (2,3) Revealed False 1 False,Square (3,1) Revealed False 0 False,Square (3,2) Revealed False 1 False,Square (3,3) Hidden True 0 False]
-}
=======
{-revealNeighbours' xxxxxxxxxxxxxxx-}
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
revealNeighbours' :: Square -> Board -> Int -> Board
revealNeighbours' (Square pos _ _ _ _) board size = mkChoice (Just pos) board board size



<<<<<<< HEAD
{-insertSquare square board
  Insert a given square into the board at its position.
  PRE: True
  VARIANT: length board
  RETURNS: The board with the square inserted.
  EXAMPLES: insertSquare (Square (1,1) Hidden False 0 False) [] = []
            insertSquare (Square (1,1) Revealed False 8 False) (mkBoard 1) = [Square (1,1) Revealed False 8 False]
=======
{-insertSquare square@(Square pos1 _ _ _) (x@(Square pos2 _ _ _):xs)
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

insertSquare :: Square -> Board -> Board
insertSquare _ [] = []
insertSquare square@(Square pos1 _ _ _ _) (x@(Square pos2 _ _ _ _):xs)
  | pos1 == pos2 = square : xs
  | otherwise = x : insertSquare square xs


<<<<<<< HEAD
{-hiddenNeighbours neighbours
Takes all the hidden neighbours from the getNeighbourSquares function.
VARIANT: Length of neighbours.
PRE: neighbours == Neighbour squares
RETURNS: A list of of all hidden neighbours in getNeighbourSquares.
EXAMPLES: hiddenNeighbours [] = []
          hiddenNeighbours (getNeighbourSquares (3,5) (mkBoard 5) 5) = [Square (2,4) Hidden False 0 False,Square (2,5) Hidden False 0 False,Square (3,4) Hidden False 0 False,Square (4,4) Hidden False 0 False,Square (4,5) Hidden False 0 False]
=======
{-getPosition (Square pos _ _ _)
-}
getPosition :: Square -> Position
getPosition (Square pos _ _ _ _) = pos


{-hiddenNeighbours neighbours
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

hiddenNeighbours :: [Square] -> [Square]
hiddenNeighbours [] = []
hiddenNeighbours (x@(Square _ Hidden _ _ _):xs) = x : hiddenNeighbours xs
hiddenNeighbours neighbours = hiddenNeighbours (tail neighbours)


{- choiceMine position board
<<<<<<< HEAD
Checks if given position has a mine on it, if it does, it returns true else false.
PRE: True.
RETURNS: A boolean telling the user whether theres a mine or not in the specific square.
EXAMPLES: choiceMine (Just(1,23)) [] = False
         choiceMine (Just(1,2)) (mkBoard 2) = False
         choiceMine (Just(1,2)) (insertMine (1,2) (mkBoard 2)) = True
=======
   Checks if given position has a mine on it, if it does, it returns true else false.
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

choiceMine :: Maybe Position -> Board -> Bool
choiceMine _ [] = False
choiceMine Nothing board = False
choiceMine (Just (x,y)) (square@(Square pos state mine mines flag):s)
  | (x,y) == pos && mine = mine
  | otherwise = choiceMine (Just (x,y)) s


<<<<<<< HEAD
{- mkFlag (Just(x,y,)) Board
   Flags a square on the board, by changing the second bool in the position.
   PRE: True.
   RETURNS: A board with a flagged position.
   EXAMPLES: mkFlag (Just(1,2)) [] = []
             mkFlag (Just(1,1)) (mkBoard 2) = [Square (1,1) Hidden False 0 True,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False]
             mkFlag (Just(0,0)) (mkBoard 2) = [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False]
-}

=======
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
mkFlag :: Maybe Position -> Board -> Board
mkFlag _ [] = []
mkFlag Nothing board = board
mkFlag (Just (x,y)) (square@(Square pos state mine mines flag):s)
  | (x,y) == pos && flag == False = (Square pos state mine mines True) : mkFlag (Just (x,y)) s
  | (x,y) == pos && flag == True = (Square pos state mine mines False) : mkFlag (Just (x,y)) s
  | otherwise = square : mkFlag (Just (x,y)) s

<<<<<<< HEAD

{- toPosition s
   This function translates a given move into its position.
   PRE: True
   RETURNS: Nothing if the move is invalid, (Just move) if it is valid
   EXAMPLES: toPosition "" = Nothing
             toPosition "asd" = Nothing
             toPosition "1a" = Just (1,1)
=======
{- toPosition s
   This function translates a given move (ex. "1f") into its position (ex. (1,6))
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

toPosition :: String -> Maybe Position
toPosition (i1:i2:c:[]) = if isDigit i1 && isDigit i2 && isAlpha c then Just (read (i1 : i2 : []) :: Int, chartoInt c cilist) else Nothing
toPosition (i:c:[]) = if isDigit i && isAlpha c then Just (digitToInt i, chartoInt c cilist) else Nothing
<<<<<<< HEAD
toPosition _ = Nothing


{- chartoInt c cilist
   Outputs the number that is mapped to the given character.
   PRE: c exists in cilist
   VARIANT: length cilist
   RETURNS: A number that represents the given character.
   EXAMPLES: chartoInt 'a' [('a',1), ('b', 2)] = 1
             chartoInt 'b' [('a',1), ('b', 2)] = 2
             chartoInt 'Z' cilist = 52
=======


{- chartoInt
   Outputs the number that is mapped to the given character.
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

chartoInt :: Char -> [(Char,Int)] -> Int
chartoInt c (x:xs)
  | c == fst x = snd x
  | otherwise = chartoInt c xs

<<<<<<< HEAD

cilist = zip (['a'..'z'] ++ ['A'..'Z']) [1..52] -- A list of tuples where characters are mappep to the corresponding numbers.
=======
{- cilist
-}
-- A list of tuples where characters are mappe to the corresponding numbers.
cilist = zip (['a'..'z'] ++ ['A'..'Z']) [1..52]
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6


{- isWin board
   Checks if all squares are revealed (not counting the squares that has mines)
<<<<<<< HEAD
   PRE: True
   VARIANT: length board
   RETURNS: True if board not containing a hidden non mine square, else False
   EXAMPLES: isWin [] = True
             isWin (mkBoard 3) = False
             isWin [Square (1,1) Revealed False 0 False] = True
=======
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

isWin :: Board -> Bool
isWin [] = True
isWin (square@(Square pos state mine mines flag):s)
  | state == Hidden && not mine = False
  | otherwise = isWin s


<<<<<<< HEAD
{- play board size
   This is the gameloop, gets a move from the user and checks if that move is a flag or not, and then insert it in the board.
   Checks if that move is a mine and if all squares are revealed, if not it calls the gameloop again.
   SIDE EFFECTS: Prints the current board and the amount of mines that exist in the board.
                 Also prints if you have won or lost the game.
   RETURNS: A string, that indicate if you have won or lost.
=======


{- play board size
   In this function we handle all the moves from the user,
   check if the game is over and prints the board. 
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

play :: Board -> Int -> IO ()
play board size = do

  putStrLn "Make your move"
<<<<<<< HEAD

  choice <- getLine

  let newBoard = board
  
  if isPrefixOf "flag " choice then do

    let newBoard = mkFlag (toPosition $ drop 5 choice) board
    prettyPrint $ printBoard newBoard size

    putStrLn $ "Amount of mines: " ++ (show $ countMines newBoard)

    play newBoard size

  else do

    let newBoard = mkChoice (toPosition choice) board board size

    prettyPrint $ printBoard newBoard size

    putStrLn $ "Amount of mines: " ++ (show $ countMines newBoard)

    if choiceMine (toPosition choice) board then putStrLn ("You Lost!\nYou hit a mine at: " ++ show choice) 

      else if (isWin newBoard) then putStrLn "You won!"

=======
  choice <- getLine

  let newBoard = board

  if isPrefixOf "flag " choice then do
    let newBoard = mkFlag (toPosition $ drop 5 choice) board
    prettyPrint $ printBoard newBoard size
    play newBoard size
  else do
    let newBoard = mkChoice (toPosition choice) board board size
    prettyPrint $ printBoard newBoard size
    if choiceMine (toPosition choice) board 
      then putStrLn ("You Lost!\nYou hit a mine at: " ++ show choice) else if (isWin newBoard) then putStrLn "You won!"
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
    else play newBoard size


{- mkDifficulty s
Outputs the size of the board and amount of mines depending on the desired difficulty
PRE: True.
RETURNS: A tuple where x is the size of the boards axes and y is the number of mines.
SIDE EFFECTS:
<<<<<<< HEAD
EXAMPLES: mkDifficulty "small" = (3,1)
=======
EXAMPLES: mkDifficulty "easy" = (3,1)
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
          mkDifficulty "Custom" = mkCustom
          mkDifficulty "Hello World!" = getDifficulty
-}

mkDifficulty :: String -> IO (Int, Int)
<<<<<<< HEAD
mkDifficulty "small" = return (3,1)
mkDifficulty "medium" = return (8,12)
mkDifficulty "large" = return (18,32)
=======
mkDifficulty "easy" = return (3,1)
mkDifficulty "medium" = return (8,8)
mkDifficulty "hard" = return (18,18)
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
mkDifficulty "custom" = mkCustom
mkDifficulty _ = getDifficulty

{- getDifficulty
<<<<<<< HEAD
   Gets a difficulty from the user, if the input is invalid, it will call the function again.
   SIDE EFFECTS: Prints the different choices the user can enter.
   RETURNS: An IO tuple that contains the size of the board and the amount of mines, of the inputed difficulty.
=======
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

getDifficulty :: IO (Int, Int)
getDifficulty = do

<<<<<<< HEAD
  putStrLn "Please enter a difficulty (small/medium/large) or choose custom"
=======
  putStrLn "Please enter a difficulty (easy/medium/hard) or choose custom"
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6

  stringDifficulty <- getLine

  difficulty <- mkDifficulty stringDifficulty

  return difficulty

<<<<<<< HEAD

{- mkCustom
   Takes custom values from the user and creates a difficulty from those values.
   RETURNS: An IO tuple with the size of board and the amount of mines.
-}

mkCustom :: IO (Int, Int)
mkCustom = do

  boardChoice <- getBoardChoice

  mineChoice <- getMineChoice boardChoice


  return (boardChoice, mineChoice)


{- getBoardChoice
   Checks if input is of valid size and if that input is of type Int.
   If not it will call the function again until it gets a valid input.
   RETURNS: The user input as an Int, if it is between range (1 - 52) and of type Int.
   SIDE EFFECTS: Prints question and error message if user input is invalid.
   EXAMPLES: 
-}

getBoardChoice :: IO Int
=======
{-mkCustom
-}

mkCustom :: IO (Int, Int)
mkCustom  = do

  boardChoice <- getBoardChoice

  let boardChoiceInt = (read boardChoice :: Int)

  mineChoice <- getMineChoice boardChoiceInt

  let mineChoiceInt = (read boardChoice :: Int)


  return (boardChoiceInt, mineChoiceInt)


{- getBoardChoice
-}

getBoardChoice :: IO String
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
getBoardChoice = do

  putStrLn "Enter the size of the board"

  boardChoice <- getLine

  let boardChoiceInt = (read boardChoice :: Int)

  if not $ checkValidInt boardChoice then do

    putStrLn "Invalid type, size is of type Int"

    getBoardChoice  

<<<<<<< HEAD
  else if boardChoiceInt > 52 || boardChoiceInt < 1 then do

    putStrLn "Invalid size, valid size is: 1 - 52"

    getBoardChoice

  else return boardChoiceInt


{- getMineChoice size
   Checks if input is of valid size and if that input is of type Int.
   If not it will call the function again until it gets a valid input.
   PRE: size >= 1
   RETURNS: The user input as an Int, if it is between range (0 - maxSize) and of type Int.
   SIDE EFFECTS: Prints question and error message if user input is invalid.
   EXAMPLES: 
-}

getMineChoice :: Int -> IO Int  
=======
  else if boardChoiceInt > 52 then do

    putStrLn "Invalid size, maximum size is: 52"

    getBoardChoice

  else return boardChoice


{- getMineChoice size
-}

getMineChoice :: Int -> IO String  
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
getMineChoice size = do

  putStrLn "Enter the number of mines on the board"

  mineChoice <- getLine

  let mineChoiceInt = (read mineChoice :: Int)

  let maxSize = size * size - 1

  if not $ checkValidInt mineChoice then do

    putStrLn "Invalid type, amount is of type Int"

    getMineChoice size

<<<<<<< HEAD
  else if mineChoiceInt > maxSize || mineChoiceInt < 0 then do

    putStrLn $ "Invalid amount, valid amount is: " ++ "0 - " ++ (show maxSize)

    getMineChoice size

  else return mineChoiceInt
=======
  else if mineChoiceInt > maxSize then do

    putStrLn $ "Invalid amount, maximum amount is: " ++ (show maxSize)

    getMineChoice size

  else return mineChoice
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6


{- checkValidInt strInt
Checks if a string consists of only integers or not.
PRE: True.
RETURNS: A boolean that tells the user if theres only integers or not in the string.
SIDE EFFECTS: True.
EXAMPLES: checkValidInt "" = False
          checkValidInt "12345" = True
          checkValidInt "1234,5" = False
          checkValidInt "Hello World!" = False
-}

checkValidInt :: String -> Bool
checkValidInt strInt = isJust (readMaybe strInt :: Maybe Int)


{- playMinesweeper
<<<<<<< HEAD
   Initializes the board with mines and the amount neighbours for each square.
   Sends the fully initialized board to the gameloop 'play'.
   SIDE EFFECTS: Prints the header and the initialized board.
=======
   Just testing stuff
>>>>>>> bfaf104b24f200b3ae0372b86b6f8677ca7c8af6
-}

playMinesweeper :: IO ()
playMinesweeper = do

    putStrLn "Welcome to Minesweeper v.1"

    (size, mineAmount) <- getDifficulty

    let board = mkBoard size

    mines <- getMines mineAmount size []

    let newBoard = insertMines mines board

    let playBoard = initNeighbours newBoard newBoard size

    prettyPrint $ printBoard playBoard size

    play playBoard size