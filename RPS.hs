-----------------------------------------------------------------------
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2010.
-- 
-- 	RPS: Rock - Paper - Scissors
-----------------------------------------------------------------------

module RPS where

import Data.Time
import System.Locale
import System.IO.Unsafe
import System.IO
import Test.QuickCheck

--
-- Basic types and functions over the type
--

-- A type of moves

data Move = Rock | 
            Paper | 
            Scissors
            deriving Eq

-- Showing Moves in an abbreviated form.

instance Show Move where
      show Rock = "r"
      show Paper = "p"
      show Scissors = "s"

-- For QuickCheck to work over the Move type.

instance Arbitrary Move where
  arbitrary     = elements [Rock, Paper, Scissors]

-- Convert from 0,1,2 to a Move

convertToMove :: Integer -> Move

convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors

-- Convert a character to the corresponding Move element.
  
convertMove :: Char -> Move
    
convertMove 'r' = Rock
convertMove 'R' = Rock
convertMove 'p' = Paper
convertMove 'P' = Paper
convertMove 's' = Scissors
convertMove 'S' = Scissors

-- Outcome of a play
--   +1 for first player wins
--   -1 for second player wins
--    0 for a draw

outcome :: Move -> Move -> Integer

outcome Rock Rock = 0
outcome Rock Paper = -1
outcome Rock Scissors = 1
outcome Paper Rock = 1
outcome Paper Paper = 0
outcome Paper Scissors = -1
outcome Scissors Rock = -1
outcome Scissors Paper = 1
outcome Scissors Scissors = 0

-- Calculating the Move to beat or lose against the 
-- argument Move.

beat, lose :: Move -> Move

beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

-- QuickCheck property about the "sanity" of the 
-- beat and lose functions.

prop_WinLose :: Move -> Bool

prop_WinLose x =
    beat x /= lose x &&
    beat x /= x &&
    lose x /= x


--
-- Strategies
--

type Strategy = [Move] -> Move

-- Random choice of Move

random :: Strategy
random _ = convertToMove $ randInt 3

-- Constant strategies

sConst :: Move -> Strategy

sConst x _ = x

rock, paper, scissors :: Strategy

rock     = sConst Rock
paper    = sConst Paper
scissors = sConst Scissors

-- Echo the previous move; also have to supply starting Move.

echo :: Move -> Strategy

echo start moves 
      = case moves of
          []       -> start
          (last:_) -> last

-- Echo a move that would have lost the last play; 
-- also have to supply starting Move.

sLostLast start moves 
      = case moves of
          [] -> start
          (last:_) -> lose last

-- Make a random choice of which Strategy to use, 
-- each turn.

sToss :: Strategy -> Strategy -> Strategy

sToss str1 str2 moves =
    case randInt 2 of
      1 -> str1 moves
      0 -> str2 moves

alternate :: Strategy -> Strategy -> Strategy

alternate str1 str2 moves =
    case length moves `rem` 2 of
      1 -> str1 moves
      0 -> str2 moves

alternate2 :: Strategy -> Strategy -> Strategy

alternate2 str1 str2 = 
    \moves ->
        case length moves `rem` 2 of
          1 -> str1 moves
          0 -> str2 moves

alternate3 :: Strategy -> Strategy -> Strategy

alternate3 str1 str2 moves = 
    map ($ moves) [str1,str2] !! (length moves `rem` 2) 

beatStrategy :: Strategy -> Strategy

beatStrategy opponent moves =
    beat (opponent moves)

--
-- Random stuff from time
--

-- Generate a random integer within the IO monad.

randomInt :: Integer -> IO Integer

randomInt n = 
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)

-- Extract the random number from the IO monad, unsafely!

randInt :: Integer -> Integer

randInt = unsafePerformIO . randomInt 


--
-- Tournaments
--

-- The Tournament type.

type Tournament = ([Move],[Move])

-- The result of a Tournament, calculates the outcome of each
-- stage and sums the results.

result :: Tournament -> Integer

result = sum . map (uncurry outcome) . uncurry zip


--
-- Play one Strategy against another
--

step :: Strategy -> Strategy -> Tournament -> Tournament

step strategyA strategyB ( movesA, movesB )
     = ( strategyA movesB : movesA , strategyB movesA : movesB )

playSvsS :: Strategy -> Strategy -> Integer -> Tournament

playSvsS strategyA strategyB n
     = if n<=0 then ([],[]) else step strategyA strategyB (playSvsS strategyA strategyB (n-1))


--
-- Playing interactively
--

-- Top-level function

play :: Strategy -> IO ()

play strategy =
    playInteractive strategy ([],[])

-- The worker function

playInteractive :: Strategy -> Tournament -> IO ()

playInteractive s t@(mine,yours) =
    do 
      ch <- getChar
      if not (ch `elem` "rpsRPS") 
        then showResults t 
        else do let next = s yours 
                putStrLn ("\nI play: " ++ show next ++ " you play: " ++ [ch])
                let yourMove = convertMove ch
                playInteractive s (next:mine, yourMove:yours)


-- Calculate the winner and report the result.

showResults :: Tournament -> IO ()

showResults t = 
    do
      let res = result t
      putStrLn (case compare res 0 of
                  GT ->  "I won!"
                  EQ -> "Draw!"
                  LT -> "You won: well done!")
      
-- Play against a randomly chosen strategy

randomPlay :: IO ()

randomPlay =
    do
      rand <- randomInt 10
      play (case rand of
            0 -> echo Paper
            1 -> sLostLast Scissors
            2 -> const Rock
            3 -> random
            4 -> sToss random (echo Paper)
            5 -> echo Rock
            6 -> sLostLast Paper
            7 -> sToss (const Rock) (const Scissors)
            8 -> const Paper
            9 -> random)
            

