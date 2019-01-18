{- Write a program that reads the name of a person and writes a nice message.

Input

The input is the name of a person.

Output

Is the name starts with an A, ‘Hello!’ must be written. Otherwise, ‘Bye!’ must be written.

Observation

In order to solve this problem in Haskell, write a main action and choose the GHC compiler. -}

main :: IO ()
main = do {
    inicial <- getChar;
    case (inicial) of 
        'A'         -> putStrLn "Hello!"
        'a'         -> putStrLn "Hello!"
        otherwise   -> putStrLn "Bye!"
}