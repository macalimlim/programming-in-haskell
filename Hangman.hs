module Hangman where

import           System.Random

main :: IO ()
main = do runGame "batman" 5

runGame :: String -> Int -> IO ()
runGame wtg n | n > 0     = do putStrLn "Guess the word: "
                               gw <- getLine
                               if wtg == gw
                                  then putStrLn "You guessed it right!!! :D"
                                  else do putStrLn "You guessed it wrong... :("
                                          runGame wtg (n - 1)
              | otherwise = putStrLn "GAME OVER..."
