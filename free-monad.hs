{-# LANGUAGE DeriveFunctor, LambdaCase #-}
import Control.Lens ((??))
import Control.Monad.Free

data FizzBuzzF a = Fizz a | Buzz a | FizzBuzz a | Number Integer a
  deriving Functor

-- Fizz buzz monad
type FizzBuzz = Free FizzBuzzF

fizzBuzz :: Integer -> FizzBuzz ()
fizzBuzz n = case f . b $ return () of
  Free (Fizz (Free (Buzz x))) -> Free (FizzBuzz x)
  Pure x -> liftF $ Number n x
  fb -> fb
  where f | n `rem` 3 == 0 = wrap . Fizz | otherwise = id
        b | n `rem` 5 == 0 = wrap . Buzz | otherwise = id

runFizzBuzz :: FizzBuzz () -> IO ()
runFizzBuzz fb = iter ?? fmap return fb $ \case
  Fizz r -> putStrLn "Fizz" >> r
  Buzz r -> putStrLn "Buzz" >> r
  FizzBuzz r -> putStrLn "FizzBuzz" >> r
  Number n r -> print n >> r

main = runFizzBuzz $ mapM_ fizzBuzz [0..100]
