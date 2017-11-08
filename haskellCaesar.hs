-- Nicholas Espinosa
-- COP 4020 - 0001
-- Haskell Caesar

import Data.Char(ord, chr, isLower)

-- Converts a letter to a number
let2nat :: Char -> Int
let2nat x = (ord x) - 97

-- Converts a number to a letter
nat2let :: Int -> Char
nat2let x = chr(x + 97)

-- Shifts forward by a given number
shift :: Int -> Char -> Char
shift n l
    | not $ isLower l = l
    | let2nat l + n > 25 = nat2let $ (let2nat l + n) `mod` 26
    | otherwise = nat2let $ let2nat l + n

-- Shifts back by a given number
unshift :: Int -> Char -> Char
unshift n l
    | not $ isLower l = l
    | let2nat l - n < 0 = nat2let $ (let2nat l - n) `mod` 26
    | otherwise = nat2let $ let2nat l - n
    
--Encodes a string using the shift function    
encode :: Int -> String -> String
encode n w = map (shift n) w

-- Decodes a string using the unshfit funtion
decode :: Int -> String -> String
decode n w = map (unshift n) w

-- Reference frequency table for further calculations
table :: [Float ]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 
        4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 
        0.2, 2.0, 0.1]
        
-- Calculates the number of lowercase letters in a string
lowers :: String -> Int
lowers w = lowerCount w 0 (length w)
    where
        lowerCount w s e
            | s == e = 0
            | isLower (w !! s) = 1 + lowerCount w (s+1) e
            | otherwise = lowerCount w (s+1) e


-- Counts the number of occurances of a letter in a string            
count :: Char -> String -> Int
count l w = letterCount l w 0 (length w)
    where
        letterCount l w s e
            | s == e = 0
            | (w !! s) == l = 1 + letterCount l w (s+1) e
            | otherwise = letterCount l w (s+1) e

-- Caulculates the percentage based upon two integers
percent :: Int -> Int -> Float
percent x y = (realToFrac x / realToFrac y) * 100.0

-- Calculates letter percentages in a string, use lowers, count, and percent
freqs :: String -> [Float]
freqs w = freqArray 0 25 w (lowers w)
    where
        freqArray s e w n
            | s > e = []
            | otherwise = (percent (count (nat2let s) w) n): freqArray (s+1) e w n

-- Rotates a list a certain amount to the left
rotate :: Int -> [a] -> [a]
rotate i w
    | i == 0 = w
    | otherwise = rotate (i-1) ((drop 1 w) ++ (take 1 w))

-- Performs the chisqr function upon two given lists
chisqr :: [Float] -> [Float] -> Float
chisqr [] [] = 0.0
chisqr (x:xs) (y:ys) = (((x - y) ^ 2) / y) + chisqr xs ys

-- Rotates a string to the left by a given number
position :: Eq a => a -> [a] -> Int
position x [] = error "Empty List"
position x (y:ys) 
    | x == y = 0
    | otherwise = 1 + position x (rotate 1 $ y:ys)

-- Cracks an encoded string
crack :: String -> String
crack w = decode (shiftAmt w) w
    where -- Determines the shift amount
        shiftAmt w = position (minimum (chiList w 0 26)) (chiList w 0 26)
            where -- Determines the chisqr of each rotation
                chiList w s e
                    | s == e = []
                    | otherwise = [(chisqr (rotate s (freqs  w)) table)] ++ chiList w (s+1) e 
