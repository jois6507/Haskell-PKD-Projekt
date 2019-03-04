-- PKD-project Minesweeper
-- Group 24
-- Made by: Johan Haag, Daniel Bäckner, Axel Nilsson


import System.Random
import Data.List
import Data.Char
import Control.Monad
import Data.Maybe
import Text.Read
import Test.HUnit hiding (State)


{-
Position represents a position on the board as a pair of coordinates represented as a tuple.
-}
type Position = (Int, Int)

{-
Mine represents the mines on the board as bools when a square is opened.
-}

type Mine = Bool

{-
NeighbourMines represent the integers on the board, which indicates how many mines there are adjacent to the current square.
-}

type NeighbourMines = Int


{- 
Flag represent if a square has been flagged or not.
-}

type Flag = Bool

{-
The type Board represent the game field as a list of Positions that creates a square.
-}
type Board = [Square]

{-
The data constructor State represents if a square is Hidden or Revealed
-}

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
          mkBoard 1 = [Square (1,1) Hidden False 0 False]
          mkBoard 3 = [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False,Square (2,3) Hidden False 0 False, Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden False 0 False]
-}

mkBoard :: Int -> Board
mkBoard n = [(Square (x,y) Hidden False 0 False) 
            | x <- [1..n], y <- [1..n]]

{- insertMines mines board
   insert a list of mines into the board. The mines are represented as bools.
PRE:
RETURNS: A list with mines inserted to the borad at positions based on the given coordinates.
SIDE EFFECTS:
VARIANT: Length of "mines" (p:ps).
EXAMPLES: insertMines [] [] = []
          insertMines [] (mkBoard 0) = []
          insertMines [(1,1)] (mkBoard 0) = []
          insertMines [(1,1), (2,3)] (mkBoard 3) = [Square (1,1) Revealed True 1 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0,Square (2,2) Revealed False 0,Square (2,3) Revealed          True 1,Square (3,1) Revealed False 0,Square (3,2) Revealed False 0,Square (3,3) Revealed False 0]
-}

insertMines :: [Position] -> Board -> Board
insertMines [] board     = board
insertMines (p:ps) []    = []
insertMines (p:ps) board = insertMines ps (insertMine p board)


{- insertMine position (square@(Square pos _ _ _):squares)
inserts a mine at the given position. 
PRE: Position has to exist.
RETURNS: A list of squares, containing one mine from position.
VARIANT: length of board
EXAMPLES: insertMine (1,6) [] = []
          insertMine (1,2) (mkBoard 2) =  [Square (1,1) Hidden False 0 False,Square (1,2) Hidden True 1 False, Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False]         
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


{- printBoard (x@(Square (x1,y1) _ _ _ _):xs) n
PRE:
RETURNS: A string representing a game board.
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
printTop 0 = []
printTop n | n < 53 = (['a'..'z'] ++ ['A'..'Z'])!!(n-1) : " " ++ printTop (n-1)
printTop n = "Sorry pal!"


{- printSquare square
Convert a square into an ASCII-character.
PRE:
RETURNS: An ASCII-character fulfilling the given condition.
SIDE EFFECTS:
EXAMPLES: printSquare (Square (1,1) Hidden False 0 False) = "#"
          printSquare (Square (2,2) Revealed False 7 False) = "7"
          printSquare (Square (2,2) Revealed False 7 True) = "F"
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
EXAMPLES: dupMines [] = False
          dupMines [(1,2), (1,5)] = False
          dupMines [(1,5),(1,5)] = True
          dupMines [(1,5),(1,5),(1,3)] = True
-}


dupMines :: [Position] -> Bool
dupMines mines = length remDups < length mines
  where remDups = map head $ group $ sort mines


{- getMines n size mines
 Returns a list of positions occupied by mines, where the positions can not be outside the boundaries of the game board.
PRE: True
VARIANT: n
RETURNS: A list of Positions where none of the elements are duplicates
EXAMPLES: getMines 0 26 [(1,1),(3,2)] = [(1,1),(3,2)]
          getMines 3 26 [] = [(7,9),(25,5),(16,2)]
          getMines 3 26 [(1,1)] = [(23,9),(8,25),(14,7),(1,1)]
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
          countMines [Square (1,1) Revealed True 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden True 0 False] = 2
          countMines [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden True 0 False] = 1
-}


countMines :: Board -> Int
countMines [] = 0 
countMines (square@(Square _ _ True _ _):s) = 1 + countMines s
countMines (square:s) = countMines s


{- getNeighbourSquares
Get all surrounding squares of the given position.
PRE: (x,y) has to be a coordinate of the board and size == size of board
RETURNS: A list of the squares that surround the  square with position (x,y)
EXAMPLES: getNeighbourSquares (2,2) Board 3 = [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden False 0 False]
-}

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
PRE: True
VARIANT: length board
RETURNS: The square in position pos
EXAMPLES: getSquare (1,1) Board = Square (1,1) Hidden False 0 False
          getSquare (2,2) Board = Square (2,2) Hidden False 0 False
-}

getSquare :: Position -> Board -> Square
getSquare pos (square@(Square spos _ _ _ _):s)
  | pos == spos = square
  | otherwise = getSquare pos s
getSquare pos board = (Square pos Hidden False 0 False)


{- countNeighbourMines (neighbour:s)
Counts all mines of neighbours.
PRE: (neighbour:s) is a list of the squares surrounding a square
VARIANT: length (neighbour:s)
RETURNS: The amount of mines present in (neighbour:s)
EXAMPLES: countNeighbourMines [Square (1,1) Hidden True 1 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden True 1 False] = 2
          countNeighbourMines [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden False 0 False] = 0
-}

countNeighbourMines :: Board -> Int
countNeighbourMines [] = 0
countNeighbourMines (neighbour:s) = (countMine neighbour) + countNeighbourMines s

{-countMine square
Gives an int that indicates wether or not the square has a mine.
PRE: True
RETURNS: 1 if Square has a mine, 0 if square does not have a mine
EXAMPLES: countMine (Square (1,1) Hidden True 1 False) = 1
          countMine (Square (2,2) Hidden False 1 False) = 0
-}

countMine :: Square -> Int
countMine (Square _ _ True _ _) = 1
countMine _ = 0


{- initNeighbours board boardcpy
This function outputs a new board with NeighbourMines initialized for all squares.
PRE: board and boardcpy are the same
VARIANT: length board
RETURNS: A board where the number contained in each square reflects the amount of mines adjacent to that square
EXAMPLES: initNeighbours [Square (1,1) Hidden True 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False] [Square (1,1) Hidden True 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False] 2 = [Square (1,1) Hidden True 0 False,Square (1,2) Hidden False 1 False,Square (2,1) Hidden False 1 False,Square (2,2) Hidden False 1 False]

-}

initNeighbours :: Board -> Board -> Int -> Board
initNeighbours [] boardcpy _ = []
initNeighbours board@(square@(Square pos state mine neighbours flag):s) boardcpy size =
             (Square pos state mine (countNeighbourMines $ getNeighbourSquares pos boardcpy size) flag : initNeighbours s boardcpy size)


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
mkChoice _ [] cpy _ = cpy
mkChoice (Just (x,y)) (square@(Square pos state mine mines flag):xs) cpy size
  | (x,y) == pos && state == Hidden && mines == 0 = revealNeighbours (hiddenNeighbours $ getNeighbourSquares pos cpy size) (insertSquare (Square pos Revealed mine mines flag) cpy) size
  | (x,y) == pos && state == Hidden = insertSquare (Square pos Revealed mine mines flag) cpy
  | otherwise = mkChoice (Just (x,y)) xs cpy size


{- revealNeighbours (x:xs) board size
Reveals all squares next to a chosen square with 0 adjacent mines, which again applies to any of the adjacent squares that also have 0 adjacent mines
PRE: (x:xs) must be a list of hidden squares
VARIANT: length board
RETURNS: A game board with the proper squares revealed
EXAMPLES: revealNeighbours [] board 3 = board
          revealNeighbours [Square (1,2) Hidden False 0 False,Square (2,2) Hidden False 0 False,Square (2,1) Hidden False 0 False] [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 1 False,Square (2,3) Hidden False 1 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 1 False,Square (3,3) Hidden True 0 False] 3 = [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 1 False,Square (2,3) Revealed False 1 False,Square (3,1) Revealed False 0 False,Square (3,2) Revealed False 1 False,Square (3,3) Hidden True 0 False]
-}

revealNeighbours :: [Square] -> Board -> Int -> Board
revealNeighbours [] board _ = board
revealNeighbours (x:xs) board size = revealNeighbours xs (revealNeighbours' x board size) size


{-revealNeighbours' 
Reveals all squares next to a chosen square with 0 adjacent mines, which again applies to any of the adjacent squares that also have 0 adjacent mines
PRE: Square is hidden
RETURNS: A board with mkChoice called on the square at position pos
EXAMPLES: revealNeighbours' (Square (1,1) Hidden False 0 False) [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 1 False,Square (2,3) Hidden False 1 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 1 False,Square (3,3) Hidden True 0 False] 3 = [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 1 False,Square (2,3) Revealed False 1 False,Square (3,1) Revealed False 0 False,Square (3,2) Revealed False 1 False,Square (3,3) Hidden True 0 False]
-}
revealNeighbours' :: Square -> Board -> Int -> Board
revealNeighbours' (Square pos _ _ _ _) board size = mkChoice (Just pos) board board size



{-insertSquare square board
  Insert a given square into the board at its position.
  PRE: True
  VARIANT: length board
  RETURNS: The board with the square inserted.
  EXAMPLES: insertSquare (Square (1,1) Hidden False 0 False) [] = []
            insertSquare (Square (1,1) Revealed False 8 False) (mkBoard 1) = [Square (1,1) Revealed False 8 False]
-}

insertSquare :: Square -> Board -> Board
insertSquare _ [] = []
insertSquare square@(Square pos1 _ _ _ _) (x@(Square pos2 _ _ _ _):xs)
  | pos1 == pos2 = square : xs
  | otherwise = x : insertSquare square xs


{-hiddenNeighbours neighbours
Takes all the hidden neighbours from the getNeighbourSquares function.
VARIANT: Length of neighbours.
PRE: neighbours == Neighbour squares
RETURNS: A list of of all hidden neighbours in getNeighbourSquares.
EXAMPLES: hiddenNeighbours [] = []
          hiddenNeighbours (getNeighbourSquares (3,5) (mkBoard 5) 5) = [Square (2,4) Hidden False 0 False,Square (2,5) Hidden False 0 False,Square (3,4) Hidden False 0 False,Square (4,4) Hidden False 0 False,Square (4,5) Hidden False 0 False]
-}

hiddenNeighbours :: [Square] -> [Square]
hiddenNeighbours [] = []
hiddenNeighbours (x@(Square _ Hidden _ _ _):xs) = x : hiddenNeighbours xs
hiddenNeighbours neighbours = hiddenNeighbours (tail neighbours)


{- choiceMine position board
Checks if given position has a mine on it, if it does, it returns true else false.
PRE: True.
RETURNS: A boolean telling the user whether theres a mine or not in the specific square.
EXAMPLES: choiceMine (Just(1,23)) [] = False
         choiceMine (Just(1,2)) (mkBoard 2) = False
         choiceMine (Just(1,2)) (insertMine (1,2) (mkBoard 2)) = True
-}

choiceMine :: Maybe Position -> Board -> Bool
choiceMine _ [] = False
choiceMine Nothing board = False
choiceMine (Just (x,y)) (square@(Square pos state mine mines flag):s)
  | (x,y) == pos && mine = mine
  | otherwise = choiceMine (Just (x,y)) s


{- mkFlag (Just(x,y,)) Board
   Flags a square on the board, by changing the second bool in the position.
   PRE: True.
   RETURNS: A board with a flagged position.
   EXAMPLES: mkFlag (Just(1,2)) [] = []
             mkFlag (Just(1,1)) (mkBoard 2) = [Square (1,1) Hidden False 0 True,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False]
             mkFlag (Just(0,0)) (mkBoard 2) = [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False]
-}

mkFlag :: Maybe Position -> Board -> Board
mkFlag _ [] = []
mkFlag Nothing board = board
mkFlag (Just (x,y)) (square@(Square pos state mine mines flag):s)
  | (x,y) == pos && flag == False = (Square pos state mine mines True) : mkFlag (Just (x,y)) s
  | (x,y) == pos && flag == True = (Square pos state mine mines False) : mkFlag (Just (x,y)) s
  | otherwise = square : mkFlag (Just (x,y)) s


{- toPosition s
   This function translates a given move into its position.
   PRE: True
   RETURNS: Nothing if the move is invalid, (Just move) if it is valid
   EXAMPLES: toPosition "" = Nothing
             toPosition "asd" = Nothing
             toPosition "1a" = Just (1,1)
-}

toPosition :: String -> Maybe Position
toPosition (i1:i2:c:[]) = if isDigit i1 && isDigit i2 && isAlpha c then Just (read (i1 : i2 : []) :: Int, chartoInt c cilist) else Nothing
toPosition (i:c:[]) = if isDigit i && isAlpha c then Just (digitToInt i, chartoInt c cilist) else Nothing
toPosition _ = Nothing


{- chartoInt c cilist
   Outputs the number that is mapped to the given character.
   PRE: c exists in cilist
   VARIANT: length cilist
   RETURNS: A number that represents the given character.
   EXAMPLES: chartoInt 'a' [('a',1), ('b', 2)] = 1
             chartoInt 'b' [('a',1), ('b', 2)] = 2
             chartoInt 'Z' cilist = 52
-}

chartoInt :: Char -> [(Char,Int)] -> Int
chartoInt c (x:xs)
  | c == fst x = snd x
  | otherwise = chartoInt c xs


cilist = zip (['a'..'z'] ++ ['A'..'Z']) [1..52] -- A list of tuples where characters are mappep to the corresponding numbers.


{- isWin board
   Checks if all squares are revealed (not counting the squares that has mines)
   PRE: True
   VARIANT: length board
   RETURNS: True if board not containing a hidden non mine square, else False
   EXAMPLES: isWin [] = True
             isWin (mkBoard 3) = False
             isWin [Square (1,1) Revealed False 0 False] = True
-}

isWin :: Board -> Bool
isWin [] = True
isWin (square@(Square pos state mine mines flag):s)
  | state == Hidden && not mine = False
  | otherwise = isWin s


{- gameLoop board size
   This is the gameloop, gets a move from the user and checks if that move is a flag or not, and then insert it in the board.
   Checks if that move is a mine and if all squares are revealed, if not it calls the gameloop again.
   SIDE EFFECTS: Prints the current board and the amount of mines that exist in the board.
                 Also prints if you have won or lost the game.
   RETURNS: A string, that indicate if you have won or lost.
-}

gameLoop :: Board -> Int -> IO ()
gameLoop board size = do

  putStrLn "Make your move"

  choice <- getLine

  let newBoard = board
  
  if isPrefixOf "flag " choice then do

    let newBoard = mkFlag (toPosition $ drop 5 choice) board
    prettyPrint $ printBoard newBoard size

    putStrLn $ "Amount of mines: " ++ (show $ countMines newBoard)

    gameLoop newBoard size

  else do

    let newBoard = mkChoice (toPosition choice) board board size

    prettyPrint $ printBoard newBoard size

    putStrLn $ "Amount of mines: " ++ (show $ countMines newBoard)

    if choiceMine (toPosition choice) board then putStrLn ("You Lost!\nYou hit a mine at: " ++ show choice) 

      else if (isWin newBoard) then putStrLn "You won!"

    else gameLoop newBoard size


{- mkDifficulty s
Outputs the size of the board and amount of mines depending on the desired difficulty
PRE: True.
RETURNS: A tuple where x is the size of the boards axes and y is the number of mines.
SIDE EFFECTS:
EXAMPLES: mkDifficulty "small" = (3,1)
          mkDifficulty "Custom" = mkCustom
          mkDifficulty "Hello World!" = getDifficulty
-}

mkDifficulty :: String -> IO (Int, Int)
mkDifficulty "small" = return (3,1)
mkDifficulty "medium" = return (8,12)
mkDifficulty "large" = return (18,32)
mkDifficulty "custom" = mkCustom
mkDifficulty _ = getDifficulty

{- getDifficulty
   Gets a difficulty from the user, if the input is invalid, it will call the function again.
   SIDE EFFECTS: Prints the different choices the user can enter.
   RETURNS: An IO tuple that contains the size of the board and the amount of mines, of the inputed difficulty.
-}

getDifficulty :: IO (Int, Int)
getDifficulty = do

  putStrLn "Please enter a difficulty (small/medium/large) or choose custom"

  stringDifficulty <- getLine

  difficulty <- mkDifficulty stringDifficulty

  return difficulty


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
getBoardChoice = do

  putStrLn "Enter the size of the board"

  boardChoice <- getLine

  let boardChoiceInt = (read boardChoice :: Int)

  if not $ checkValidInt boardChoice then do

    putStrLn "Invalid type, size is of type Int"

    getBoardChoice  

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
getMineChoice size = do

  putStrLn "Enter the number of mines on the board"

  mineChoice <- getLine

  let mineChoiceInt = (read mineChoice :: Int)

  let maxSize = size * size - 1

  if not $ checkValidInt mineChoice then do

    putStrLn "Invalid type, amount is of type Int"

    getMineChoice size

  else if mineChoiceInt > maxSize || mineChoiceInt < 0 then do

    putStrLn $ "Invalid amount, valid amount is: " ++ "0 - " ++ (show maxSize)

    getMineChoice size

  else return mineChoiceInt


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
   Initializes the board with mines and the amount neighbours for each square.
   Sends the fully initialized board to the gameloop 'play'.
   SIDE EFFECTS: Prints the header and the initialized board.
-}

playMinesweeper :: IO ()
playMinesweeper = do

    putStrLn "Welcome to Minesweeper (final version)"

    (size, mineAmount) <- getDifficulty

    let board = mkBoard size

    mines <- getMines mineAmount size []

    let newBoard = insertMines mines board

    let playBoard = initNeighbours newBoard newBoard size

    prettyPrint $ printBoard playBoard size

    gameLoop playBoard size


-- mkBoard
test1 = TestCase (assertEqual "for (mkBoard 0)," [] (mkBoard 0))
test2 = TestCase (assertEqual "for (mkBoard (-1))," [] (mkBoard (-1)))
test3 = TestCase (assertEqual "for (mkBoard 3)," [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False,Square (2,3) Hidden False 0 False, Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden False 0 False] (mkBoard 3))

-- insertMines
test4 = TestCase (assertEqual "for (insertMines [] (mkBoard 2))," (mkBoard 2) (insertMines [] (mkBoard 2)))
test5 = TestCase (assertEqual "for (insertMines [(1,1), (2,2), (3,3)] (mkBoard 3))," [Square (1,1) Hidden True 1 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden True 1 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden True 1 False]
 (insertMines [(1,1), (2,2), (3,3)] (mkBoard 3)))

-- printBoard
test6 = TestCase (assertEqual "for (printBoard [] 0)," "" (printBoard [] 0))
test7 = TestCase (assertEqual "for (printBoard (mkBoard 4) 4)," "    a b c d\n1 | # # # #\n2 | # # # #\n3 | # # # #\n4 | # # # #\n" (printBoard (mkBoard 4) 4))

-- printSquare
test8 = TestCase (assertEqual "for (printSquare (Square (1,1) Revealed False 0 False))," " " (printSquare (Square (1,1) Revealed False 0 False)))
test9 = TestCase (assertEqual "for (printSquare (Square (1,1) Revealed False 8 True))," "F" (printSquare (Square (1,1) Revealed False 8 True)))

-- dupMines
test10 = TestCase (assertEqual "for (dupMines [(1,1), (1,2), (1,3)])," False (dupMines [(1,1), (1,2), (1,3)]))
test11 = TestCase (assertEqual "for (dupMines [(1,1), (2,2), (1,1)])," True (dupMines [(1,1), (2,2), (1,1)]))

-- countMines
test12 = TestCase (assertEqual "for (countMines (mkBoard 3))," 0 (countMines (mkBoard 3)))
test13 = TestCase (assertEqual "for (countMines $ insertMines [(1,1), (2,2), (3,3)] (mkBoard 3))," 3 (countMines $ insertMines [(1,1), (2,2), (3,3)] (mkBoard 3)))

-- getNeighbourSquares
test14 = TestCase (assertEqual "for (getNeighbourSquares (1,1) (mkBoard 3) 3)," [Square (1,2) Hidden False 0 False,Square (2,2) Hidden False 0 False,Square (2,1) Hidden False 0 False] (getNeighbourSquares (1,1) (mkBoard 3) 3))
test15 = TestCase (assertEqual "for (getNeighbourSquares (2,2) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 3)) 3)," [Square (1,1) Hidden True 1 False,Square (1,2) Hidden False 0 False,Square (1,3) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,3) Hidden False 0 False,Square (3,1) Hidden False 0 False,Square (3,2) Hidden False 0 False,Square (3,3) Hidden True 1 False] (getNeighbourSquares (2,2) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 3)) 3))

-- initNeighbours
test16 = TestCase (assertEqual "for (initNeighbours (mkBoard 3) (mkBoard 3) 3)," (mkBoard 3) (initNeighbours (mkBoard 3) (mkBoard 3) 3))
test17 = TestCase (assertEqual "for (initNeighbours (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) 4)," [Square (1,1) Hidden True 1 False,Square (1,2) Hidden False 2 False,Square (1,3) Hidden False 1 False,Square (1,4) Hidden False 0 False,Square (2,1) Hidden False 2 False,Square (2,2) Hidden True 2 False,Square (2,3) Hidden False 2 False,Square (2,4) Hidden False 1 False,Square (3,1) Hidden False 1 False,Square (3,2) Hidden False 2 False,Square (3,3) Hidden True 1 False,Square (3,4) Hidden False 1 False,Square (4,1) Hidden False 0 False,Square (4,2) Hidden False 1 False,Square (4,3) Hidden False 1 False,Square (4,4) Hidden False 1 False] (initNeighbours (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) 4))

-- mkChoice
test18 = TestCase (assertEqual "for (mkChoice Nothing [] [] 0)," [] (mkChoice Nothing [] [] 0))
test19 = TestCase (assertEqual "for (mkChoice (Just (1,1)) (mkBoard 3) (mkBoard 3) 3)," [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (1,3) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 0 False,Square (2,3) Revealed False 0 False,Square (3,1) Revealed False 0 False,Square (3,2) Revealed False 0 False,Square (3,3) Revealed False 0 False] (mkChoice (Just (1,1)) (mkBoard 3) (mkBoard 3) 3))
test20 = TestCase (assertEqual "for (mkChoice (Just (4,4)) (initNeighbours (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) 4) (initNeighbours (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) 4) 4)," [Square (1,1) Hidden True 1 False,Square (1,2) Hidden False 2 False,Square (1,3) Hidden False 1 False,Square (1,4) Hidden False 0 False,Square (2,1) Hidden False 2 False,Square (2,2) Hidden True 2 False,Square (2,3) Hidden False 2 False,Square (2,4) Hidden False 1 False,Square (3,1) Hidden False 1 False,Square (3,2) Hidden False 2 False,Square (3,3) Hidden True 1 False,Square (3,4) Hidden False 1 False,Square (4,1) Hidden False 0 False,Square (4,2) Hidden False 1 False,Square (4,3) Hidden False 1 False,Square (4,4) Revealed False 1 False] (mkChoice (Just (4,4)) (initNeighbours (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) 4) (initNeighbours (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) (insertMines [(1,1), (2,2), (3,3)] (mkBoard 4)) 4) 4))

-- mkFlag
test21 = TestCase (assertEqual "for (mkFlag Nothing (mkBoard 2))," [Square (1,1) Hidden False 0 False,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False] (mkFlag Nothing (mkBoard 2)))
test22 = TestCase (assertEqual "for (mkFlag (Just (1,1)) (mkBoard 2))," [Square (1,1) Hidden False 0 True,Square (1,2) Hidden False 0 False,Square (2,1) Hidden False 0 False,Square (2,2) Hidden False 0 False] (mkFlag (Just (1,1)) (mkBoard 2)))

-- toPosition
test23 = TestCase (assertEqual "for (toPosition \"\")," Nothing (toPosition ""))
test24 = TestCase (assertEqual "for (toPosition \"a1\")," Nothing (toPosition "a1"))
test25 = TestCase (assertEqual "for (toPosition \"1b\")," (Just (1,2)) (toPosition "1b"))

-- isWin
test26 = TestCase (assertEqual "for (isWin (mkBoard 3))," False (isWin (mkBoard 3)))
test27 = TestCase (assertEqual "for (isWin (insertMines [(1,1)] (mkBoard 3)))," False (isWin (insertMines [(1,1)] (mkBoard 3))))
test28 = TestCase (assertEqual "for (isWin [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 0 False])," True (isWin [Square (1,1) Revealed False 0 False,Square (1,2) Revealed False 0 False,Square (2,1) Revealed False 0 False,Square (2,2) Revealed False 0 False]))



tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
                  test11, test12, test13, test14, test15, test16, test17, test18, test19, test20,
                  test21, test22, test23, test24, test25, test26, test27, test28]



runTests = runTestTT tests