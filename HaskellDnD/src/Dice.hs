module Dice(
  Dice(..),
  d4,
  d6,
  d8,
  d20
) where
import System.Random

--Attempting to make Dice as a mondad. 
-- Takes int for # of sides, and a tuple of an int
-- for the remaining sides of the dice.
newtype Dice a = Dice { roll :: Int -> (a, Int) }

--Functor lets me map a func over a wrapped value; defines how map works for Dice
--Simple, as rolling a die has limited outcomes.
--Takes the function f, and dice D, returns new dice where f applies to d.
instance Functor Dice where
  fmap f (Dice d) = Dice (\n -> let (x, n') = d n in (f x, n'))

--Applicative needed to define what <*> does. Applies dice func to a dice value.
--Pure wraps the value in 'Dice' 
-- <*>Takes 2 die values, takes the first int and applies it to the 2nd as a func.
instance Applicative Dice where
  pure x = Dice (\n -> (x, n))
  (Dice df) <*> (Dice da) = Dice (\n ->
    let (f, n') = df n
        (a, n'') = da n' in (f a, n''))

--For dice to be a real monad, needs a 'return' and >>= to qualify.
--As a monad, it allows us to chain dice rolls, super useful in the future.

instance Monad Dice where
  --Return is the same as pure.
  return = pure
  -- >>= takes Dice value, function of dice value, returns a dice value.
  (Dice da) >>= f = Dice (\n ->
    --Applies the dice-wrapped function and returns the new dice value.
    let (a, n') = da n
        (Dice db) = f a in db n')

d4 :: Dice Int
d4 = Dice (\n -> let (x, gen') = randomR (1, 4) (mkStdGen n) in (x, fromIntegral $ fst (next gen')))

d6 :: Dice Int
d6 = Dice (\n -> let (x, gen') = randomR (1, 6) (mkStdGen n) in (x, fromIntegral $ fst (next gen')))

d8 :: Dice Int
d8 = Dice (\n -> let (x, gen') = randomR (1, 8) (mkStdGen n) in (x, fromIntegral $ fst (next gen')))

d20 :: Dice Int
d20 = Dice (\n -> let (x, gen') = randomR (1, 20) (mkStdGen n) in (x, fromIntegral $ fst (next gen')))