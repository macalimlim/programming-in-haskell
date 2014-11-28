module Chapter9 where

{-

Interactive Programs (IO monad)

to interact with the outside world (with side effects)

In the early days, Haskell cant deal with IO

Input --> Program --> Output

getChar :: IO Char

putChar :: Char -> IO ()

return :: a -> IO a

getLine :: IO String
getLine = do x <- getChar
             if x == '\n'
                then return []
                else do xs <- getLine
                        return (x : xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do putChar x
                     putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do putStr x
                 putChar '\n'

-}

strLen :: IO ()
strLen = do putStr "Enter a string: "
            xs <- getLine
            putStrLn ("The string has " ++ (show (length xs)) ++ " characters")

strLen' :: IO ()
strLen' = putStr "Enter a string: " >>=
            (\ _ -> getLine >>=
            (\ xs -> putStrLn ("The string has " ++ (show (length xs)) ++ " characters")))
