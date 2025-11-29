-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment1 ( put,
                     moveLeft,
                     moveRight,
                     moveUp,
                     moveDown,
                     Grid(..),
                     GridWithAPointer(..),
                     putTatamiDown,
                     putTatamiUp,
                     putTatamiLeft,
                     putTatamiRight,
                     cover
             ) where

import Data.Char (isLetter)

-- these two function are to correctly measure the width of an entry of a grid, 
-- i.e. so that the width of "\ESC[44m55\ESC[0m" ignored the escape sequences
stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':xs) = stripANSI (drop 1 (dropWhile (not . isLetter) xs))
stripANSI (x:xs) = x : stripANSI xs

visibleLength :: String -> Int
visibleLength = length . stripANSI

newtype Grid a = Grid { grid :: [[a]] } deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid g)
    | null g = ""
    | otherwise = unlines (map showRow g)
    where
      strGrid = map (map show) g
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths (map show row)]
      padRight n s = s ++ replicate (n - visibleLength s) ' '

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


newtype GridWithAPointer a = GridWithAPointer (Grid a, [a], a, [a], Grid a)
  deriving Eq


---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- TASK 1
---------------------------------------------------------------------------------

instance Show a => Show (GridWithAPointer a) where
    show (GridWithAPointer (Grid up, ls, p, rs, Grid down)) =
        let rowStr row = unwords (map show row)
            currRow = unwords $ map show ls ++ [show p] ++ map show rs
        in unlines $ map rowStr up ++ [currRow] ++ map rowStr down


---------------------------------------------------------------------------------
-- TASK 2
---------------------------------------------------------------------------------

put :: a -> GridWithAPointer a -> GridWithAPointer a
put val (GridWithAPointer (up, leftElems, _, rightElems, down)) =
    GridWithAPointer (up, leftElems, val, rightElems, down)

moveLeft :: GridWithAPointer a -> GridWithAPointer a
moveLeft (GridWithAPointer (up, l:ls, p, rs, down)) =
    GridWithAPointer (up, ls, l, p:rs, down)
moveLeft gwp = gwp

moveRight :: GridWithAPointer a -> GridWithAPointer a
moveRight (GridWithAPointer (up, ls, p, r:rs, down)) =
    GridWithAPointer (up, ls ++ [p], r, rs, down)
moveRight gwp = gwp

moveUp :: GridWithAPointer a -> GridWithAPointer a
moveUp (GridWithAPointer (Grid (upRows), leftElems, p, rightElems, down)) =
    case reverse upRows of
      (rowAbove:restUp) ->
          let newPointer = rowAbove !! length leftElems
              newLeft = take (length leftElems) rowAbove
              newRight = drop (length leftElems + 1) rowAbove
              newUp = Grid (reverse restUp)
              newDownRow = leftElems ++ [p] ++ rightElems
          in GridWithAPointer (newUp, newLeft, newPointer, newRight, Grid (newDownRow : grid down))
      [] -> GridWithAPointer (Grid (upRows), leftElems, p, rightElems, down)

moveDown :: GridWithAPointer a -> GridWithAPointer a
moveDown (GridWithAPointer (up, leftElems, p, rightElems, Grid (downRows))) =
    case downRows of
      (rowBelow:restDown) ->
          let newPointer = rowBelow !! length leftElems
              newLeft = take (length leftElems) rowBelow
              newRight = drop (length leftElems + 1) rowBelow
              newUpRow = leftElems ++ [p] ++ rightElems
          in GridWithAPointer (Grid (grid up ++ [newUpRow]), newLeft, newPointer, newRight, Grid restDown)
      [] -> GridWithAPointer (up, leftElems, p, rightElems, Grid (downRows))  -- 到底，不动

---------------------------------------------------------------------------------
-- TASK 3
---------------------------------------------------------------------------------

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 newVal (_:xs) = newVal : xs
replaceNth n newVal (x:xs) = x : replaceNth (n-1) newVal xs

replaceFirstN :: Int -> a -> [a] -> [a]
replaceFirstN 0 _ xs = xs
replaceFirstN _ _ [] = []
replaceFirstN n val (_:xs) = val : replaceFirstN (n-1) val xs

replaceLastN :: Int -> a -> [a] -> [a]
replaceLastN n val xs =
    let (front, back) = splitAt (length xs - n) xs
    in front ++ map (const val) back

replaceCol :: Int -> a -> [a] -> [a]
replaceCol idx val row =
    take idx row ++ [val] ++ drop (idx+1) row

putTatamiUp :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiUp n (GridWithAPointer (Grid upRows, ls, p, rs, down)) =
    let colIdx = length ls
        n' = fromIntegral n
        numRows = length upRows
        start = max 0 (numRows - n')
        newUpRows = zipWith (\row i ->
                                if i >= start
                                then replaceNth colIdx n row
                                else row
                             ) upRows [0..]
        newP = n
    in GridWithAPointer (Grid newUpRows, ls, newP, rs, down)


putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown n (GridWithAPointer (up, ls, p, rs, Grid downRows)) =
    let colIdx = length ls
        n' = fromIntegral n
        (toChange, rest) = splitAt n' downRows
        newDownRows = map (\row -> replaceCol colIdx n row) toChange ++ rest
        newP = n
    in GridWithAPointer (up, ls, newP, rs, Grid newDownRows)


putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight n (GridWithAPointer (up, ls, p, rs, down)) =
    let n' = fromIntegral n
        newRight = replaceFirstN n' n rs
        newP = n
    in GridWithAPointer (up, ls, newP, newRight, down)


putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft n (GridWithAPointer (up, ls, p, rs, down)) =
    let n' = fromIntegral n
        newLeft = replaceLastN n' n ls
        newP = n
    in GridWithAPointer (up, newLeft, newP, rs, down)
    
---------------------------------------------------------------------------------
-- TASK 4
---------------------------------------------------------------------------------

cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover (GridWithAPointer (Grid g, _, _, _, _)) =
    let m = length g
        n = if null g then 0 else length (head g)
    in if odd m && odd n
       then error "Cannot cover grid with tatamis (odd x odd)"
       else let filled = fillBlocks g 1
            in if validateGrid filled
               then toGridWithPointer filled
               else error "Invalid tatami placement"

fillBlocks :: [[Integer]] -> Integer -> [[Integer]]
fillBlocks grid start =
    let m = length grid
        n = if null grid then 0 else length (head grid)
        nextNum = start
        fillBlock i j num =
            let positions = [(i,j),(i,j+1),(i+1,j),(i+1,j+1)]
                nums = [num, num+1, num+2, num+2]
            in zip positions nums

        blocks = [ fillBlock i j (nextNum + 4*((toInteger i `div` 2)*(toInteger n `div` 2) + (toInteger j `div` 2))) 
                 | i <- [0,2..m-2], j <- [0,2..n-2] ]
    in mergeBlocks m n (concat blocks)

mergeBlocks :: Int -> Int -> [((Int,Int),Integer)] -> [[Integer]]
mergeBlocks m n posList =
    [ [ lookupCell i j posList | j <- [0..n-1] ] | i <- [0..m-1] ]
  where
    lookupCell i j lst =
        case lookup (i,j) lst of
          Just v -> v
          Nothing -> 0

validateGrid :: [[Integer]] -> Bool
validateGrid g =
    let m = length g
        n = if null g then 0 else length (head g)
        blocks = [ [(g!!i!!j),(g!!i!!(j+1)),(g!!(i+1)!!j),(g!!(i+1)!!(j+1))]
                 | i <- [0,2..m-2], j <- [0,2..n-2] ]
    in all (\b -> length (unique b) <= 3) blocks

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/=x) xs)

toGridWithPointer :: [[Integer]] -> GridWithAPointer Integer
toGridWithPointer rows =
    case rows of
      [] -> error "Empty grid"
      (r0:rs) ->
        let p = head r0
            rsRow = tail r0
        in GridWithAPointer (Grid [], [], p, rsRow, Grid rs)
