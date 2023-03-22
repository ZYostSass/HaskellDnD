module Dice where

--Attempting to make Dice as a mondad. 
--Acts as a wrapper around a traditional 'Int'
  newtype Dice a = Dice { roll :: Int -> (a, Int) }

--Functor lets me map a func over a wrapped value; defines how map works for Dice
--Simple, as rolling a die has limited outcomes.
instance Functor Dice where
  fmap f (Dice d) = Dice (\n -> let (x, n') = d n in (f x, n'))

--Applicative needed to define what <*> does.
--Pure is the wrapper for the n value to Dice. 
--Takes 2 die values, takes the first int and applies it to the 2nd as a func.
--Finally, the = Dice wraps it back up in the Dice constructor.
instance Applicative Dice where
  pure x = Dice (\n -> (x, n))
  (Dice df) <*> (Dice da) = Dice (\n ->
    let (f, n') = df n
        (a, n'') = da n' in (f a, n''))

--For dice to be a real monad, needs a 'return' and >>= to qualify.
--As a monad, it allows us to chain dice rolls, super useful in the future.

instance Monad Dice where
  return = pure
  (Dice da) >>= f = Dice (\n ->
    let (a, n') = da n
        (Dice db) = f a in db n')
