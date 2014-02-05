import Data.IORef
import Control.Monad
import System.IO.Unsafe

main = do
  i <- newIORef 1
  loop i
    where
      loop i = do
      v <- readIORef i

      fizzBuzz v

      if v == 100
        then return ()
        else do
          writeIORef i (v+1)
          loop i

fizzBuzz n = do
  let mod3 = (mod n 3 == 0)

  when mod3           $ putStr "Fizz"
  if (mod n 5 == 0)
    then putStr "Buzz"
    else unless mod3  $ putStr $ show n

  putChar '\n'
