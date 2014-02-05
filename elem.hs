{-# LANGUAGE ViewPatterns #-}

fizzBuzz :: Integer -> String
fizzBuzz m@((`mod` 15) -> n)
  | n == 0              = "Fizzbuzz"
  | n `elem` [3,6,9,12] = "Fizz"
  | n `elem` [5,10]     = "Buzz"
  | otherwise           = show m

main = mapM_ (putStrLn . fizzBuzz) [1..100]
