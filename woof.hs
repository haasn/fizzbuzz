import Control.Applicative
import Control.Lens
import Data.Char (intToDigit)

fizzBuzzWoof :: Int -> String
fizzBuzzWoof = view $ mconcat [3 ~> "Fizz", 5 ~> "Buzz", 7 ~> "Woof"] `failing` to show
  where n ~> s = mconcat [to show.each.only (intToDigit n), to (`rem` n).only 0].to (const s)

main = mapM_ (putStrLn . fizzBuzzWoof) [1..100]
