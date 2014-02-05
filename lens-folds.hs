import Control.Applicative
import Control.Lens

fizzBuzz :: Integer -> String
fizzBuzz = view $ mconcat [3 ~> "fizz", 5 ~> "buzz"] `failing` to show
  where n ~> s = to (`mod` n).only 0.to (const s)

main = mapM_ (putStrLn . fizzBuzz) [1..100]
