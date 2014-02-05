{-# LANGUAGE MonadComprehensions #-}
import Data.Maybe
import Data.Monoid

fizzBuzz x = fromMaybe (show x) $ ["fizz" | x `rem` 3 == 0] <> ["buzz" | x `rem` 5 == 0]
