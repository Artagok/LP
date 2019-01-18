{-
Write a Haskell program to interpret the body mass index of several individuals.
Input
Input is organized in lines. Each line has three elements separated with whitespaces: the name, the weight and the height. The last line is special and only contains an asterisc.
Output
For each individual, print his/her name and the interpretation of his/her BMI.
Observation
In order to solve this problem in Haskell, write a main action and choose the GHC compiler. 
-}

main :: IO ()
main = do {
    line <- getLine;
    if (line /= "*")
        then do {
            process line;
            main
        }
        else putStr ""
}

process line = 
    putStrLn $ (string !! 0) ++ ": " ++ bmi (read (string !! 1)::Float) (read (string !! 2)::Float)
    where string = words line

bmi weight height
    | r < 18              = "underweight"
    | r >= 18 && r < 25   = "normal weight"
    | r >= 25 && r < 30   = "overweight"
    | r >= 30 && r < 40   = "obese"
    | r > 40              = "severely obese"
    where
        r = weight / (height ^ 2)