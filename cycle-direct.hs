fizzBuzz n "" "" = print n
fizzBuzz _ xs ys = mapM_ putStr [xs, ys, "\n"]

main = sequence_ $ zipWith3 fizzBuzz [1..100] fizz buzz
  where fizz = cycle ["","","fizz"]; buzz = cycle ["","","","","buzz"]
