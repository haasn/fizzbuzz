{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances, GADTs
  , TypeSynonymInstances, MultiParamTypeClasses, OverlappingInstances
  , FlexibleInstances, ScopedTypeVariables, TypeOperators, PolyKinds #-}

import GHC.TypeLits

data Proxy (p :: k) = Proxy

data N = Z | S N

-- lisp ahoy
type A10 n = S (S (S (S (S (S (S (S (S (S n)))))))))
type N100 = A10 (A10 (A10 (A10 (A10 (A10 (A10 (A10 (A10 (A10 Z)))))))))

foldn :: (a -> a) -> a -> N -> a
foldn _ z Z     = z
foldn f z (S n) = f (foldn f z n)

data instance Sing (n :: N) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)

instance SingI Z where sing = SZ
instance SingI n => SingI (S n) where sing = SS sing

instance SingE (KindParam :: OfKind N) where
  type DemoteRep (KindParam :: OfKind N) = N
  fromSing SZ = Z
  fromSing (SS n) = S (fromSing n)

class Mod3 (n :: N) where
  mod3 :: proxy n -> Bool

class Mod5 (n :: N) where
  mod5 :: proxy n -> Bool

instance Mod3 Z where mod3 _ = True
instance Mod5 Z where mod5 _ = True

instance Mod3 n => Mod3 (S (S (S n))) where mod3 _ = mod3 (Proxy :: Proxy n)
instance Mod5 n => Mod5 (S (S (S (S (S n))))) where mod5 _ = mod5 (Proxy :: Proxy n)

instance Mod3 n where mod3 _ = False
instance Mod5 n where mod5 _ = False

class FizzBuzz (n :: k) where
  fizzBuzz :: proxy n -> String

instance (Mod3 n, Mod5 n, SingRep n, DemoteRep (KindOf n) ~ N) => FizzBuzz (n :: N) where
  fizzBuzz p = case f mod3 "fizz" ++ f mod5 "buzz" of
    "" -> show . foldn succ (0 :: Int) $ fromSing (sing :: Sing n)
    s  -> s
   where f x s = if x p then s else ""

instance FizzBuzz '[] where
  fizzBuzz _ = ""

instance (FizzBuzz n, FizzBuzz ns) => FizzBuzz (n ': ns) where
  fizzBuzz _ = fizzBuzz (Proxy :: Proxy n) ++ "\n" ++ fizzBuzz (Proxy :: Proxy ns)

type family EnumFromTo (n :: N) (m :: N) :: [N]
type instance where
  EnumFromTo m m = '[]
  EnumFromTo n m = n ': EnumFromTo (S n) m

main = putStrLn $ fizzBuzz (Proxy :: Proxy (EnumFromTo Z N100))
