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
-- Conversion overview:
--   Rose a        is a tree with branches:    Lf a  |  Br [Rose a]
--   Free [] a     is the same structure:      Pure a  |  Free [Free [] a]
--
--   toRose   : Free [] a  -> Rose a
--   fromRose : Rose a     -> Free [] a
--
--   These functions should form an isomorphism:
--       toRose (fromRose r) = r
--       fromRose (toRose f) = f

-- Rose tree:
--        Br
--      /     \
--   Lf 1     Br
--           /   \
--        Lf 2   Lf 3
--
-- r = Br [Lf 1, Br [Lf 2, Lf 3]]

-- Example Free [] a term with the same shape:
-- f = Free [Pure 1, Free [Pure 2, Pure 3]]

-- Since Rose has no Eq instance, we define a structural equality for testing:
--   eqRose (Lf x) (Lf y)       = x == y
--   eqRose (Br xs) (Br ys)     = length xs == length ys
--                                && and (zipWith eqRose xs ys)
--   eqRose _ _                 = False

-- Test (Rose -> Free -> Rose):
--   eqRose (toRose (fromRose r)) r

-- Test (Free -> Rose -> Free), comparing through Rose:
--   eqRose (toRose f) (toRose (fromRose (toRose f)))

-- 
toRose :: Free [] a -> Rose a
toRose (Pure x)= Lf x
toRose (Free xs) = Br (map toRose xs)

fromRose :: Rose a -> Free [] a
fromRose (Lf x)= Pure x
fromRose (Br xs) = Free (map fromRose xs)

{- Question 2 -}
-- I don't know why showing this error so i change the name
-- Assignment3.hs:73:5: error:
--     Ambiguous occurrence ‘trace’
--     It could refer to
--        either ‘Types.trace’,
--               imported from ‘Types’ at Assignment3.hs:10:1-12
--               (and originally defined at Types.hs:72:1-5)
--            or ‘Assignment3.trace’, defined at Assignment3.hs:67:1
--    |
-- 73 |     trace next
--    |     ^^^^^
-- Failed, one module loaded.

trace :: FreeState s a -> State ([s], s) a
trace (Pure x) = return x

trace (Free a) = do
    (logs, st) <- get                    -- Get the current log and state
    let (next, st') = runState a st       -- Run the computation a with the state st
    put (st' : logs, st')                -- Update the logs and state
    trace next                           -- Recursively call trace with the next computation

{- Question 3 -}

roundRobin :: [YieldState s ()] -> State s ()
roundRobin [] = return () -- No processes left to run -> return ()

roundRobin (p: ps) = case p of 
    Pure () -> roundRobin ps -- When a computation finishes with a Pure, that process is removed from the list

    Free (FLeft st) -> do
        next <- st --st is a State s (YieldState s ())
        roundRobin (ps ++ [next]) -- Add the yielded process to the end of the list

    Free (FRight (Yield next)) -> roundRobin (ps ++ [next]) -- release the yield and continue
    

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

    -- Run thread i until:
    --   • it executes a Sleep
    --   • or finishes (Pure)
    --
    -- Every State-step is 1 tick → other threads' counters -1

    runThread
      :: Int
      -> [(Int, SleepState s ())]
      -> State s [(Int, SleepState s ())]
    runThread i ts =
      let (before,(c,t):after) = splitAt i ts
      in case t of

        -- Thread ends
        Pure () -> return (before ++ after)

        -- State-step: consumes 1 tick
        --   • execute the effect
        --   • this thread becomes "next"
        --   • other threads sleepCounter-- 
        Free (FLeft st) -> do
          next <- st
          let ts' = zipWith update [0..] ts
              update j (c', t')
                | j == i    = (0, next)
                | otherwise = (max 0 (c' - 1), t')
          -- Keep running same thread i
          runThread i ts'

        -- Sleep n: DOES NOT consume tick
        -- scheduler resumes to outer loop

        Free (FRight (Sleep n next)) ->
          return (before ++ [(n, next)] ++ after)
