data N = Z | S N

instance Show N where
  show = show . toInt
    where toInt Z     = 0
          toInt (S n) = 1 + toInt n

fizz, buzz, fizzbuzz :: N -> String
fizz Z = "fizz"
fizz (S (S (S n))) = fizz n
fizz n = ""

buzz Z = "buzz"
buzz (S (S (S (S (S n))))) = buzz n
buzz n = ""

fizzbuzz n = case fizz n ++ buzz n of
  "" -> show n
  s  -> s

main = mapM_ (putStrLn . fizzbuzz) . take 100 $ iterate S (S Z)
