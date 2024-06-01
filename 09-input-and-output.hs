{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
import Data.List
import System.Environment
import System.Random

skipNewLine :: [String] -> Bool
skipNewLine args = head args == "-n"

asString :: [String] -> String
asString args 
  | skipNewLine args = unwords $ tail args
  | otherwise = unwords args ++ "\n" 
  
main = do
  args <- getArgs
  putStr $ asString args

 --Write a lottery number picker
 --This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
lottery :: StdGen -> [Int]
lottery gen = (sort . take 6 . nub) $ randomRs (1, 49) gen
