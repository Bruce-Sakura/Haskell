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
roundRobin = undefined

{- Question 4 -}

schedule :: [SleepState s ()] -> State s ()
schedule = undefined
