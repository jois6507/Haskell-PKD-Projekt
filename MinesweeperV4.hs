 

import System.Random

import Data.List

import Data.Char

import Control.Monad

import Data.Maybe

import Text.Read

import Debug.Trace



type Position = (Int, Int)

type Mine = Bool

type NeighbourMines = Int



data State = Revealed 

           | Hidden deriving (Show, Eq)



data Square = Square Position State Mine NeighbourMines deriving (Show,Eq)



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

{- insertMine position (square@(Square pos _ _ _):squares)

PRE: Position has to exist.

RETURNS: A list of squares, containing one mine from position.

SIDE EFFECTS:

VARIANT: ?

EXAMPLES: insertMine (1,6) [] = []

          insertMine (1,2) (mkBoard 2) =  [Square (1,1) Hidden False 0,Square (1,2) Hidden True 1,Square (2,1) Hidden False 0,Square (2,2) Hidden False 0]

          

 -} 

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

PRE: True.

RETURNS: A string without ' "" '.

SIDE EFFECTS:

EXAMPLES: prettyPrint "" =

          prettyPrint "Hello World!" = Hello World!

          prettyPrint "I love numbers: 123234" = I love numbers: 123234  

-}

prettyPrint :: String -> IO ()

prettyPrint n = putStrLn n

{- printBoard (x@(Square (x1,y1) _ _ _):xs) n







-}

printBoard :: Board -> Int -> String

printBoard []     n = []

printBoard (x@(Square (x1,y1) _ _ _):xs) n

  | y1 == 1 && x1 == 1 = "   " ++ (reverse $ printTop n) ++ "\n" ++ (show x1) ++ " | " ++ printSquare x ++ " " ++ printBoard xs n

  | y1 == 1 = if x1 < 10 then (show x1) ++ " | " ++ printSquare x ++ " " ++ printBoard xs n 

    else (show x1) ++ "| " ++ printSquare x ++ " " ++ printBoard xs n

  | y1 == n = printSquare x ++ "\n" ++ printBoard xs n

  | otherwise = printSquare x ++ " " ++ printBoard xs n


{- printTop n

Prints the alphabet in reversed order with the first n letters.

PRE: True.

RETURNS: A string with some of the alphabet, in reversed order.

SIDE EFFECTS:

EXAMPLES: printTop 0 = ""

          printTop 26 = "z y x w v u t s r q p o n m l k j i h g f e d c b a "

          printTop 27 = "Nice try! The alphabet is not that long"     

-}
printTop :: Int -> String

printTop 0 = []

printTop n = (['a'..'z'] ++ ['A'..'Z'])!!(n-1) : " " ++ printTop (n-1)



{- printSquare square

   Convert a square into an ASCII-character

-}

printSquare :: Square -> String

printSquare (Square _ Hidden _ _) = "#"

printSquare (Square _ Revealed True n) = "*"

printSquare (Square _ Revealed _ 0) = " "

printSquare (Square _ Revealed _ n) = show n


{-dupMines mines





-}

dupMines :: [Position] -> Bool

dupMines mines = length remDups < length mines

  where remDups = map head $ group $ sort mines

{- getMines n size mines

-}

getMines :: Int -> Int -> [Position] -> IO [Position]

getMines 0 size mines = return mines

getMines n size mines = do

  mine <- randomCoord size

  let mineList = (mine:mines)



  if (dupMines mineList) then getMines n size mines

    else getMines (n-1) size mineList




{- countMines (square@(Square _ _ True _):s)





-}

countMines :: Board -> Int

countMines [] = 0 

countMines (square@(Square _ _ True _):s) = 1 + countMines s

countMines (square:s) = countMines s



{- getNeighbourSquares

   get all surrounding squares of hte given position

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

  | otherwise =   [ (getSquare (x-1,y-1) board),

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

getSquare pos (square@(Square spos state _ _):s)

  | pos == spos = square

  | otherwise = getSquare pos s



{- countNeighbourMines neighbours

   counts all mines of neighbours

-}

countNeighbourMines :: Board -> Int

countNeighbourMines [] = 0

countNeighbourMines (neighbour:s) = (countMine neighbour) + countNeighbourMines s




{-countMine (Square _ _ _ _





-}
-- If square has a mine, this will output 1 else 0

countMine :: Square -> Int

countMine (Square _ _ True _) = 1

countMine _ = 0



{- initNeighbours board boardcpy

   This function outputs a new board with NeighbourMines initialized for all squares.

-}

initNeighbours :: Board -> Board -> Int -> Board

initNeighbours [] boardcpy _ = []

initNeighbours board@(square@(Square pos state mine neighbours):s) boardcpy size =

             (Square pos state mine (countNeighbourMines $ getNeighbourSquares pos boardcpy size): initNeighbours s boardcpy size)



{- mkChoice position board

   This function takes a position and reveal the state of that square.

-}

mkChoice :: Maybe Position -> Board -> Board -> Int -> Board

mkChoice _ [] cpy _ = cpy

mkChoice (Just (x,y)) (square@(Square pos state mine mines):xs) cpy size

  | (x,y) == pos && state == Hidden && mines == 0 = revealNeighbours (hiddenNeighbours $ getNeighbourSquares pos cpy size) (insertSquare (Square pos Revealed mine mines) cpy) size

  | (x,y) == pos && state == Hidden = insertSquare (Square pos Revealed mine mines) cpy

  | otherwise = mkChoice (Just (x,y)) xs cpy size

{- revealNeighbours COORDINATES!?!?!?  board size

-}

revealNeighbours :: [Square] -> Board -> Int -> Board

revealNeighbours [] board _ = board

revealNeighbours (x:xs) board size = revealNeighbours xs (revealNeighbours' x board size) size


{-revealNeighbours' xxxxxxxxxxxxxxx-}
revealNeighbours' :: Square -> Board -> Int -> Board

revealNeighbours' (Square pos _ _ _) board size = mkChoice (Just pos) board board size

{-insertSquare square@(Square pos1 _ _ _) (x@(Square pos2 _ _ _):xs)


-}


insertSquare :: Square -> Board -> Board

insertSquare _ [] = []

insertSquare square@(Square pos1 _ _ _) (x@(Square pos2 _ _ _):xs)

  | pos1 == pos2 = square : xs

  | otherwise = x : insertSquare square xs

{-getPosition (Square pos _ _ _)

-}

getPosition :: Square -> Position

getPosition (Square pos _ _ _) = pos

{-hiddenNeighbours neighbours

-}

hiddenNeighbours :: [Square] -> [Square]

hiddenNeighbours [] = []

hiddenNeighbours (x@(Square _ Hidden _ _):xs) = x : hiddenNeighbours xs

hiddenNeighbours neighbours = hiddenNeighbours (tail neighbours)



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

{- cilist



-}

-- A list of tuples where characters are mappe to the corresponding numbers.

cilist = zip (['a'..'z'] ++ ['A'..'Z']) [1..52]





{- isWin board

   Checks if all squares are revealed (not counting the squares that has mines)

-}

isWin :: Board -> Bool

isWin [] = True

isWin (square@(Square pos state mine mines):s)

  | state == Hidden && not mine = False

  | otherwise = isWin s



{- play board size

   In this function we handle all the moves from the user,

   check if the game is over and prints the board. 

-}

play :: Board -> Int -> IO ()

play board size = do



  putStrLn "Make your move"



  choice <- getLine



  let newBoard = mkChoice (toPosition choice) board board size



  prettyPrint $ printBoard newBoard size



  if choiceMine (toPosition choice) board 

    then putStrLn ("You Lost!\nYou hit a mine at: " ++ show choice) else if (isWin newBoard) then putStrLn "You won!"

      else play newBoard size



{- mkDifficulty s

   Outputs the size of the board and amount of mines depending on the desired difficulty

-}

mkDifficulty :: String -> IO (Int, Int)

mkDifficulty "easy" = return (3,1)

mkDifficulty "medium" = return (8,8)

mkDifficulty "hard" = return (18,18)

mkDifficulty "custom" = mkCustom

mkDifficulty _ = getDifficulty


{- getDifficulty

-}

getDifficulty :: IO (Int, Int)
getDifficulty = do
  putStrLn "Please enter a difficulty (easy/medium/hard) or choose custom"

  stringDifficulty <- getLine

  difficulty <- mkDifficulty stringDifficulty

  return difficulty


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
getBoardChoice = do
  putStrLn "Enter the size of the board"
  boardChoice <- getLine
  let boardChoiceInt = (read boardChoice :: Int)
  if not $ checkValidInt boardChoice then do
    putStrLn "Invalid type, size is of type Int"
    getBoardChoice  
  else if boardChoiceInt > 52 then do
    putStrLn "Invalid size, maximum size is: 52"
    getBoardChoice
  else return boardChoice
  

{- getMineChoice size

-}

getMineChoice :: Int -> IO String  
getMineChoice size = do
  putStrLn "Enter the number of mines on the board"
  mineChoice <- getLine
  let mineChoiceInt = (read mineChoice :: Int)
  let maxSize = size * size - 1
  if not $ checkValidInt mineChoice then do
    putStrLn "Invalid type, amount is of type Int"
    getMineChoice size
  else if mineChoiceInt > maxSize then do
    putStrLn $ "Invalid size, maximum size is: " ++ (show maxSize)
    getMineChoice size
  else return mineChoice


{- checkValidInt strInt

-}

checkValidInt :: String -> Bool
checkValidInt strInt = isJust (readMaybe strInt :: Maybe Int)


{- playMinesweeper

   Just testing stuff

-}

playMinesweeper :: IO ()

playMinesweeper = do

    putStrLn "Welcome to Minesweeper v.0.9"


    (size, mineAmount) <- getDifficulty


    let board = mkBoard size



    mines <- getMines mineAmount size []



    let newBoard = insertMines mines board

    let playBoard = initNeighbours newBoard newBoard size



    prettyPrint $ printBoard playBoard size



    play playBoard size
