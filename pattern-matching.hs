fizzbuzz :: Integer -> String
fizzbuzz n = case (n `rem` 3, n `rem` 5) of
  (0, 0) -> "fizzbuzz"
  (0, _) -> "fizz"
  (_, 0) -> "buzz"
  (_, _) -> show n

main = mapM_ (putStrLn . fizzbuzz) [0..100]
