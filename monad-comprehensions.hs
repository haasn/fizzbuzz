{-# LANGUAGE MonadComprehensions #-}
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

fizzBuzz :: Integer -> String
fizzBuzz x = fromMaybe (show x) $ ["fizz" | x `rem` 3 == 0] <> ["buzz" | x `rem` 5 == 0]

main = mapM_ (putStrLn . fizzBuzz) [1..100]
