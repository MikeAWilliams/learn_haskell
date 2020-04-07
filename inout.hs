main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
    putStrLn "Enter some other text and it will be the final value of the program"
    getLine