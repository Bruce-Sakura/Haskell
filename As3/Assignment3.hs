-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment3 (toRose, fromRose, trace, roundRobin, schedule) where

import Types
import Control.Monad.State
import Data.Functor.Identity
import Data.List 

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
-- Rose tree:
--        Br
--      /     \
--   Lf 1     Br
--           /   \
--        Lf 2   Lf 3
 
toRose :: Free [] a -> Rose a
toRose (Pure x)= Lf x
toRose (Free xs) = Br (map toRose xs)

fromRose :: Rose a -> Free [] a
fromRose (Lf x)= Pure x
fromRose (Br xs) = Free (map fromRose xs)

{- Question 2 -}
trace :: FreeState s a -> State ([s], s) a
trace (Pure x) = return x

trace (Free a) = do
    (logs, st) <- get                    -- Get the current log and state
    let (next, st') = runState a st       -- Run the computation a with the state st
    put (st' : logs, st')                -- Update the logs and state
    trace next                           -- Recursively call trace with the next computation

{- Question 3 -}
roundRobin :: [YieldState s ()] -> State s ()
roundRobin [] = return ()

roundRobin (p : ps) =
  case p of

    -- Thread ends → remove from list
    Pure () ->
      roundRobin ps

    -- State instruction (does NOT yield)
    Free (FLeft st) -> do
      next <- st
      -- continue running *the same thread*
      roundRobin (next : ps)

    -- Yield instruction → move to back
    Free (FRight (Yield next)) ->
      roundRobin (ps ++ [next])

charWriter :: Char -> YieldState String ()
charWriter c = do 
  s <- getY
  if length s > 10 
    then pure ()
    else do
      putY (c:s)
      yield
      charWriter c

yieldExample :: [YieldState String ()]
yieldExample = [charWriter 'a', charWriter 'b', charWriter 'c']

{- Question 4 -}
schedule :: [SleepState s ()] -> State s ()
schedule ts0 = loop (initThreads ts0)
  where
    -- Represent each thread as (sleepCounter, threadState)
    initThreads :: [SleepState s ()] -> [(Int, SleepState s ())]
    initThreads = map (\t -> (0,t))
    
    -- Main scheduling loop
    loop :: [(Int, SleepState s ())] -> State s ()
    loop [] = return ()     -- no threads → finish

    loop ts =
      case firstRunnable ts of
        Nothing ->
          -- all sleeping → advance time
          loop (map dec ts)
        Just i ->
          -- run thread i until it sleeps or exits
          runThread i ts >>= loop

    -- Find first runnable thread (sleepCounter == 0)
    firstRunnable :: [(Int, SleepState s ())] -> Maybe Int
    firstRunnable = go 0
      where
        go _ [] = Nothing
        go i ((c,_):xs)
          | c == 0    = Just i
          | otherwise = go (i+1) xs

    -- Decrement a sleep counter by 1 but not below zero
    dec :: (Int, SleepState s ()) -> (Int, SleepState s ())
    dec (c,t) = (max 0 (c-1), t)

    runThread
      :: Int
      -> [(Int, SleepState s ())]
      -> State s [(Int, SleepState s ())]
    runThread i ts =
      let (before,(c,t):after) = splitAt i ts
      in case t of

        -- Thread ends
        Pure () -> return (before ++ after)

        Free (FLeft st) -> do
          next <- st
          let ts' = zipWith update [0..] ts
              update j (c', t')
                | j == i    = (0, next)
                | otherwise = (max 0 (c' - 1), t')

          runThread i ts'

        Free (FRight (Sleep n next)) ->
          return (before ++ [(n, next)] ++ after)
