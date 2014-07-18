---------------------
-- Towers of Hanoi --
---------------------

import Control.Monad.Writer
import Data.List

data Pole = A | B | C deriving (Show)

--       ==        ||        ||
--      ====       ||        ||
--     ======      ||        ||
--    --------  --------  --------
--       A         B         C

hanoi' :: Pole -> Pole -> Pole -> Int -> Writer [String] Int
hanoi' source spare target n
  | n == 1    = writer (1, [show source ++ " -> " ++ show target])
  | otherwise =
      do
        -- Move (n-1) tower from source to spare
        a <- hanoi' source target spare (n-1)
        -- Move the size n disk from source to target
        hanoi' source spare target 1
        -- Move the (n-1) tower from the spare to the target
        b <- hanoi' spare source target (n-1)
        -- Return the correct number of moves
        return (1 + a + b)

printSolution :: (Int, [String]) -> IO ()
printSolution (n, moves) = do
  putStrLn ""
  putStrLn $ "Solved with " ++ show n ++ " moves."
  putStrLn $ intercalate ", " moves
  putStrLn ""

------------------------------------

hanoi :: Int -> IO ()
hanoi n = printSolution $ runWriter (hanoi' A B C n)