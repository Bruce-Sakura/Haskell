-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment2 (encodeWord , encodeWords , encodeText ,
                    decodeText ,
                    decodeTextWithTree ,
                    ramify ,
                    tabulate ,
                    tree) where

import Types
import Data.List
---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------
-- use for test Q6
-- import Data.Maybe

{- Question 1 -}


encodeWord :: Table -> String -> Code
-- use lookup to shearch in list
encodeWord table st = 
    concat $ intersperse shortGap [code | c <- st , Just code <- [lookup c table]]


encodeWords :: Table -> [String] -> Code
-- -- spilt :: a -> [a] -> [[a]] spilt encodeWords
encodeWords table words = 
    concat $ intersperse mediumGap [encodeWord table c | c <- words]

encodeText :: Table -> String -> Code
-- make text to words
-- list comprehension
-- [ expression | pattern <- list, condition1, condition2, ... ]
-- ghci> words "HELLO WORLD" = ["HELLO","WORLD"]
encodeText table text = 
    concat $ intersperse mediumGap [encodeWords table [w] | w <- words text]

-- anather way
-- encodeText table text = 
--     concat $ intersperse mediumGap [encodeWord table w | w <- words text]

{- Question 2 -}

letterGap :: Code
letterGap = replicate 3 Silence

splitShortGap :: Code -> [Code]
splitShortGap = go []
  where
    go acc [] = [reverse acc]
    go acc lst
      | letterGap `isPrefixOf` lst =
          let acc' = Silence : acc                      
              rest' = drop (length letterGap) lst   --remove 2 silence
          in reverse acc' : go [] rest'
      | otherwise = go (head lst : acc) (tail lst)


wordGap :: Code
wordGap = replicate 7 Silence

splitMediumGap :: Code -> [Code]
splitMediumGap = go []
  where
    go acc [] = [reverse acc]
    go acc lst
      | wordGap `isPrefixOf` lst =
          let acc' = Silence : acc
              rest' = drop (length wordGap) lst
          in reverse acc' : go [] rest'
      | otherwise = go (head lst : acc) (tail lst)

-- search in the table
findChar :: Table -> Code -> Maybe Char
findChar [] _ = Nothing
findChar ((wd, cd):xs) target
  | cd == target = Just wd
  | otherwise    = findChar xs target

decodeWord :: Table -> Code -> String
decodeWord table wordCode =
  [ c | code <- splitShortGap wordCode
      , Just c <- [findChar table code] ]


decodeText :: Table -> Code -> String
decodeText table code = unwords (map (decodeWord table) (splitMediumGap code))


-- These ont work
-- 3 silence but 1 is for letter -2
-- letterShortGap :: Code
-- letterShortGap = replicate 3 Silence


-- splitShortGap :: Code -> [Code]
-- splitShortGap = go []
--   where
--     go acc [] = [reverse acc]
--     go acc lst
--       | letterShortGap `isPrefixOf` lst = reverse acc : go [] (drop (length letterShortGap) lst)
--       | otherwise = go (head lst : acc) (tail lst)

-- decodeText table wordCode =
--   [ b | code <- splitShortGap wordCode
--       , Just b <- 
--   [c | code <- splitMediumGap wordCode,
--         Just c <- [findChar table code]]]

-- breakOnShortGap :: Code -> (Code, Code)
-- breakOnShortGap [] = ([], [])
-- breakOnShortGap cd =
--     let (pre, rest) = spanList cd
--     in (pre, rest)

-- splitShortGap :: Code -> [Code]
-- splitShortGap [] = []
-- splitShortGap wordCode = 
--     let (start,rest) = breakOnShortGap wordCode
--     in start : splitShortGap (drop (length shortGap) rest)

-- splitShortGap :: Code -> (Code, Code)
-- splitShortGap wd = scan [] wd
--     where
--         scan acc [] = (reverse acc ,[])
--         scan acc rest@(x:xs)
--             | shortGap `isPrefixOf` rest = (reverse acc, rest)
--             | otherwise = scan (x:acc) xs

-- spiltMediumGap :: Code -> [Code]
-- spiltMediumGap [] = []
-- spiltMediumGap wordsCode = 
--     let (start,rest) = breakOnMediumGap wordsCode
--     in start : spiltMediumGap (drop (lenght mediumGap) rest)


{- Question 3 -}
-- morseTree :: Tree
-- morseTree = Branch Nothing (Branch (Just 'E') (Branch (Just 'I') (Branch (Just 'S') (Branch (Just 'H') (Branch (Just '5') Empty Empty) (Branch (Just '4') Empty Empty)) (Branch (Just 'V') Empty (Branch (Just '3') Empty Empty))) (Branch (Just 'U') (Branch (Just 'F') Empty Empty) (Branch Nothing Empty (Branch (Just '2') Empty Empty)))) (Branch (Just 'A') (Branch (Just 'R') (Branch (Just 'L') Empty Empty) Empty) (Branch (Just 'W') (Branch (Just 'P') Empty Empty) (Branch (Just 'J') Empty (Branch (Just '1') Empty Empty))))) (Branch (Just 'T') (Branch (Just 'N') (Branch (Just 'D') (Branch (Just 'B') (Branch (Just '6') Empty Empty) Empty) (Branch (Just 'X') Empty Empty)) (Branch (Just 'K') (Branch (Just 'C') Empty Empty) (Branch (Just 'Y') Empty Empty))) (Branch (Just 'M') (Branch (Just 'G') (Branch (Just 'Z') (Branch (Just '7') Empty Empty) Empty) (Branch (Just 'Q') Empty Empty)) (Branch (Just 'O') (Branch Nothing (Branch (Just '8') Empty Empty) Empty) (Branch Nothing (Branch (Just '9') Empty Empty) (Branch (Just '0') Empty Empty)))))

decodeTextWithTree :: Tree -> Code -> String
-- first split words using tree
decodeTextWithTree tree code =
    unwords [decodeWordWithTree tree w | w <- splitMediumGap code]
    -- it should be like [[Atom],[Atom]]
  where

    -- decode word
    decodeWordWithTree :: Tree -> Code -> String
    decodeWordWithTree t word =
      -- it will split the word to letter
      [c | letter <- splitShortGap word
          , Just c <- [decodeLetterWithTree t letter]]

    -- this is the tree
    decodeLetterWithTree :: Tree -> Code -> Maybe Char
    decodeLetterWithTree Empty _ = Nothing
    decodeLetterWithTree (Branch c _ _) [] = c
    decodeLetterWithTree (Branch _ l r) xs
      | dit `isPrefixOf` xs  = decodeLetterWithTree l (drop (length dit) xs)
      | dah `isPrefixOf` xs  = decodeLetterWithTree r (drop (length dah) xs)
      | otherwise            = Nothing


{- Question 4 -}

ramify :: Table -> Tree
ramify table = foldl insert Empty table
  where
-- input(char, code) in the table
    insert :: Tree -> (Char, Code) -> Tree
    insert tree (ch, code) = insertAt tree code
      where

        insertAt :: Tree -> Code -> Tree

        -- if code is empty put word in to tree
        insertAt Empty [] = Branch (Just ch) Empty Empty
        insertAt (Branch _ l r) [] = Branch (Just ch) l r

        -- if is empty need to add a new branch
        -- this is a tree
        insertAt Empty xs
          | dit `isPrefixOf` xs =
              Branch Nothing
                (insertAt Empty (drop (length dit) xs))
                Empty

          | dah `isPrefixOf` xs =
              Branch Nothing
                Empty
                (insertAt Empty (drop (length dah) xs))

          | otherwise = Empty -- this is if no code it will end

        -- xs is mose code
        -- if xs == dit, then drop it and insert to the branch
        -- c is current code, l is left tree, r is right tree
        insertAt (Branch c l r) xs
          | dit `isPrefixOf` xs =
              Branch c
                (insertAt l (drop (length dit) xs))
                r
          
          -- if xs == dah, then drop dah ...
          | dah `isPrefixOf` xs =
              Branch c
                l
                (insertAt r (drop (length dah) xs))

          | otherwise = Branch c l r
          

{- Question 5 -}
-- t = ramify morseTable

tabulate :: Tree -> Table
tabulate tree = go [] tree
  where
    go :: Code -> Tree -> Table
    go _ Empty = [] -- empty 

    go path (Branch Nothing l r) = go (path ++ dit) l ++ go (path ++ dah) r

    go path (Branch (Just c) l r) = (c, path) : go (path ++ dit) l ++ go (path ++ dah) r

{- Question 6 -}

-- for test
-- xs = "({}())"

brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

--  if xs is empty > Nothing
--  if the first later is '(' > parseRound
--  if the first later is'{' > parseCurly
--  else > Nothing

tree :: String -> Maybe Bracket
tree xs =
  case parseBracket xs of
    Just (b, "") -> Just b
    _            -> Nothing
  where

    -- parse a single bracket block
    parseBracket :: String -> Maybe (Bracket, String)
    parseBracket [] = Nothing

    -- Round block
    parseBracket ('(' : rest) =
      case parseChildren rest of
        Just (children, ')' : rest2) ->
          Just (Round children, rest2)
        _ -> Nothing

    -- Curly block
    parseBracket ('{' : rest) =
      case parseChildren rest of
        Just (children, '}' : rest2) ->
          Just (Curly children, rest2)
        _ -> Nothing

    parseBracket _ = Nothing


    -- parse zero or more bracket blocks inside (…) or {…}
    parseChildren :: String -> Maybe ([Bracket], String)
    parseChildren xs =
      case parseBracket xs of
        Just (b, rest) ->
          case parseChildren rest of
            Just (bs, rest2) -> Just (b:bs, rest2)
            Nothing -> Nothing
        -- no more blocks → children list ends
        Nothing -> Just ([], xs)

-- this is a test
isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True
