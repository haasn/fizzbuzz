import Control.Applicative
import Control.Monad (guard)
import Control.Parallel.Strategies

import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import Data.Maybe  (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Vector as V (sequence_)

fizzBuzz :: (Show a, Integral a) => a -> String
fizzBuzz x = fromMaybe (show x) $ fizz <> buzz
 where (fizz, buzz) = ( "fizz" <$ guard (x `rem` 3 == 0)
                      , "buzz" <$ guard (x `rem` 5 == 0)
                      ) -- `using` parTuple2 rseq rseq
{-# SPECIALIZE fizzBuzz :: Int -> String #-}

{-
main = do
  r <- computeVectorP $ fromFunction (ix1 1000000) (\(Z:.i) -> putStrLn $ fizzBuzz i)
  V.sequence_ $ toVector r
-}

main = mapM_ (putStrLn . fizzBuzz) [1..1000000]
