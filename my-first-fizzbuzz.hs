data FizzBuzz a = FizzBuzz a

instance (Num a) => Num (FizzBuzz a)
  where
    (FizzBuzz a) + (FizzBuzz b) = FizzBuzz (a+b)
    (FizzBuzz a) * (FizzBuzz b) = FizzBuzz (a*b)
    abs (FizzBuzz a) = FizzBuzz (abs a)
    signum (FizzBuzz a) = FizzBuzz (signum a)
    fromInteger a = FizzBuzz (fromInteger a)

instance (Integral a, Show a) => Show (FizzBuzz a)
  where
    show (FizzBuzz a) = output $ fizz ++ buzz
      where
        output [] = show a
        output fb = fb

        fizz | a `mod` 3 == 0 = "fizz"
             | otherwise  = ""

        buzz | a `mod` 5 == 0 = "buzz"
             | otherwise  = ""


instance (Enum a) => Enum (FizzBuzz a)
  where
    toEnum = FizzBuzz . toEnum
    fromEnum (FizzBuzz a) = fromEnum a


main = mapM_ print [1..100 :: (Num a) => FizzBuzz a]
