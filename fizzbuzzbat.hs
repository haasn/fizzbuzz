import Control.Applicative
import Control.Lens
import Control.Monad (guard)

import Data.Char (toUpper)
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)

fizzBuzz :: Integer -> String
fizzBuzz x = fromMaybe (show x) $ mconcat
  [ "fizz" <$ guard (x `rem` 3 == 0)
  , "buzz" <$ guard (x `rem` 5 == 0)
  , "bat"  <$ guard (x `rem` 7 == 0)
  ] & _Just._head %~ toUpper

main = mapM_ (putStrLn . fizzBuzz) [1..100]
