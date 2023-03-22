module Dice where

--I'm having a lot of trouble importing outside libraries....

--import System.Random

--Attempting to make Dice as a mondad. 
--Acts as a wrapper around a traditional 'Int'
newtype Die n = Die { unwrapDie :: Int }

--Functor lets me map a func over a wrapped value; defines how map works for Die
--Simple, as rolling a die has limited outcomes.
instance Functor Die where
  fmap f (Die n) = Die n

--Applicative needed to define what <*> does.
--Pure is the wrapper for the n value to Die. 
--Takes 2 die values, takes the first int and applies it to the 2nd as a func.
--Finally, the = Die wraps it back up in the Die constructor.
instance Applicative Die where
  pure n = Die n
  (Die f) <*> (Die x) = Die (f x)

--For dice to be a real monad, needs a 'return' and >>= to qualify.
--As a monad, it allows us to chain dice rolls, super useful in the future.
instance Monad Die where
  return x = Die (return x)
  (Die d) >>= f = Die (d >>= (unDie . f))

roll :: Int -> Int -> Die Int
roll numDie numSides = Die $ do
  gen <- newStdGen
  let rolls = take numDie $ randomRs (1, numSides) gen
  return $ sum rolls