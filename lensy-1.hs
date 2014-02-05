import Control.Lens

fizzBuzz :: Integer -> String
fizzBuzz x = [(3,"fizz"), (5,"buzz")] ^. failing fizz buzz where
  fizz = each.itraversed.indices (\n -> x `rem` n == 0)
  buzz = to.const $ show x

main = mapM_ (putStrLn . fizzBuzz) [1..100]
