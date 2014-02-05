fizzBuzz :: Show a => a -> String -> String -> String
fizzBuzz n "" "" = show n
fizzBuzz _ xs ys = xs ++ ys

main = putStrLn . unlines $ zipWith3 fizzBuzz [1..100] fizz buzz
  where fizz = cycle ["","","fizz"]
        buzz = cycle ["","","","","buzz"]
