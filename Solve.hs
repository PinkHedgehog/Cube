{-# LANGUAGE OverloadedStrings #-}
module Solve where

import  qualified Data.ByteString as B
import Data.Word
import Cube
import Control.Monad (when, unless, forM_, forM)
import qualified Data.ByteString.Char8 as D (putStr, putStrLn, writeFile)
import System.Exit
import Data.Array.Unboxed
import Data.List (iterate)
{-
turnrs :: B.ByteString
turnrs = "RST"

turnfs :: B.ByteString
turnfs = "FGH"

turnus :: B.ByteString
turnus = "UVW"

--65-90 = A-Z, 97-122 = a-z

turnas = "RSTUVWFGH"
-}
lToTurn :: Word8 -> Cube -> Cube
lToTurn 82 = turnR
lToTurn 83 = turnS
lToTurn 84 = turnT
lToTurn 85 = turnU
lToTurn 86 = turnV
lToTurn 87 = turnW
lToTurn 70 = turnF
lToTurn 71 = turnG
lToTurn 72 = turnH
{-
lToTurn 114 = rotX
lToTurn 115 = rotX'
lToTurn 116 = rotX2
lToTurn 117 = rotY
lToTurn 118 = rotY'
lToTurn 119 = rotY2
lToTurn 102 = rotZ
lToTurn 103 = rotZ'
lToTurn 104 = rotZ2
-}
lToTurn _  = id

mplTurns :: Cube -> B.ByteString -> Cube
{-#INLINE mplTurns #-}
mplTurns = B.foldl' (flip lToTurn)

mplTurns' :: Cube -> B.ByteString -> Cube
{-#INLINE mplTurns' #-}
mplTurns' = B.foldr' lToTurn



{-
нужна функция, которая для заданного числа n генерирует цепочки перестановок длины
n, причём хвостовым методом. Пока что - просто следующую цепочку, зная последний элемент.
Т.е. функции нужно число и строка, с которой что-то хотим сделать
-}

nextChar :: Word8 -> Word8
nextChar c | (c >= 82) && (c < 87) || (c > 69) && (c < 72) = c + 1
           | c >= 87 = 70
           | otherwise = 82

nextOrientedChar :: Word8 -> Word8
nextOrientedChar 84 = 85
nextOrientedChar 85 = 86
nextOrientedChar 86 = 87
nextOrientedChar 87 = 72
nextOrientedChar 72 = 84

nextOriented :: B.ByteString -> B.ByteString
nextOriented bstring | B.all (== 72) bstring = odds !! (B.length bstring)
                     | B.head bstring == 72 = nextOrientedChar b `B.cons` (nextOriented $! a)
                     | otherwise = B.cons (nextOrientedChar b) a
    where
        (b, a) = fromMaybe $ B.uncons bstring
        fromMaybe Nothing  = (0, "")
        fromMaybe (Just x) = x
        len = B.length bstring
        odds = ["T", "UT", "TUT", "UTUT", "TUTUT", "UTUTUT", "TUTUTUT", "UTUTUTUT", "TUTUTUTUT", "UTUTUTUTUT", "TUTUTUTUTUT"] 

permute :: Cube -> Word8 -> B.ByteString -> B.ByteString -> IO ()
permute cube n prev bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (solved cb) $ do
            D.putStr . solShow $ prev
            D.putStr . solShow $ bstr
            D.putStrLn "solved"
    when (B.length bstr < fromIntegral n) $ do
        permute cube n prev (nextOriented bstr)

{-
solve :: Cube -> B.ByteString -> IO ()
--solve _    ""      = putStrLn "Cube is probably solved"
solve cube bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (solved cb) $ do
            D.putStrLn . solShow $ bstr
    when (B.length bstr < 12) $ do
        when (B.length bstr < B.length (next bstr)) $ do
            putStrLn . show $ B.length bstr
        solve cube (next bstr)
-}

next :: B.ByteString -> B.ByteString
next bstring | B.length bstring >= 12 = undefined
             | B.all ( == (72 :: Word8)) bstring = odds !! len
             | B.head bstring == 72 = nextChar b `B.cons` (next $! a)
             | otherwise = B.cons (nextChar b) a
    where
        (b, a) = fromMaybe $ B.uncons bstring
        fromMaybe Nothing  = (0, "")
        fromMaybe (Just x) = x
        len = B.length bstring
        odds = ["R", "UR", "RUR", "URUR", "RURUR", "URURUR", "RURURUR", "URURURUR", "RURURURUR", "URURURURUR", "RURURURURUR"]

hasPair :: B.ByteString -> Bool
hasPair str | B.length str < 2 = False
hasPair bstr = or $ map (\i->isRight (i,bstr) || isFrontal (i,bstr) || isUp (i,bstr)) [0..(B.length bstr - 2)]
    where
        isRight (i, b) = (b ! i) >= 82 && (b ! i) <= 84 && (b ! (i+1)) >= 82 && (b ! (i+1)) <= 84
        isFrontal (i, b) = (b ! i) >= 70 && (b ! i) <= 72 && (b ! (i+1)) >= 70 && (b ! (i+1)) <= 72
        isUp (i, b) = (b ! i) >= 85 && (b ! i) <= 87 && (b ! (i+1)) >= 85 && (b ! (i+1)) <= 87
        (!) = B.index

solShow :: B.ByteString -> B.ByteString
{-# INLINE solShow #-}
solShow = B.concatMap conv

conv :: Word8 -> B.ByteString
conv 82 = "R "
conv 83 = "R' "
conv 84 = "R2 "
conv 85 = "U "
conv 86 = "U' "
conv 87 = "U2 "
conv 70 = "F "
conv 71 = "F' "
conv 72 = "F2 "

{-
    [ id, rotY, rotY', rotY2
    , rotX, rotY . rotX, rotY2 . rotX, rotY' . rotX
    , rotX', rotY . rotX', rotY2 . rotX', rotY' . rotX'
    , rotX2, rotY . rotX2, rotY2 . rotX2, rotY' . rotX2
    , rotZ, rotY . rotZ, rotY2 . rotZ, rotY' . rotZ
    , rotZ', rotY . rotZ', rotY2 . rotZ', rotY' . rotZ'
    ]
-}
rotToOrient :: Cube -> (B.ByteString, (Cube -> Cube))
rotToOrient cube | oriented cube = ("", id)
rotToOrient (Cube arr) | mod (arr ! 8) 10 == 0 = ("", id)
                       | mod (arr ! 8) 10 == 1 = ("x y ", rotY . rotX)
                       | otherwise = ("z' y' ", rotZ' . rotY')


orient :: Cube -> Word8 -> B.ByteString -> IO ()
--solve _    ""      = putStrLn "Cube is probably solved"
orient cube n bstr = do
    unless (hasPair bstr) $ do
        let cb  = mplTurns cube bstr
        when (oriented cb) $ do
            --D.putStr . solShow $ bstr
            when (solved cb) $ do
                D.putStr bstr
                D.putStr "solved"
            unless (solved cb) $ do
                permute cb n bstr "T"
            --D.putStrLn ""
    when (B.length bstr < 12) $ do
        when (B.length bstr < B.length (next bstr)) $ do
            putStrLn . show $ B.length bstr
        orient cube n (next bstr)

solve :: Cube -> B.ByteString -> IO ()
--solve _    ""      = putStrLn "Cube is probably solved"
solve cube bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (solved cb) $ do
            D.putStrLn . solShow $ bstr
    when (B.length bstr < 12) $ do
        when (B.length bstr < B.length (next bstr)) $ do
            putStrLn . show $ B.length bstr
        solve cube (next bstr)
--    when (B.length bstr == 6 && B.all ( == (72 :: Word8)) bstr ) $ do {---photolab-}
--        solve' cb bstr "R"

{-
solve' :: Cube -> B.ByteString -> B.ByteString -> IO ()
solve' cube bstr rev = do
    unless (hasPair bstr) $ do
        let cb2 = mplTurns initCube rev
        when (cube == cb2) $ do
            D.putStr bstr
            D.putStr (B.reverse rev)
    when (B.length rev <= 6) $ do
        solve' cube bstr (next rev)
-}

revturn :: Word8 -> Word8
revturn 82 = 83
revturn 83 = 82
revturn 70 = 71
revturn 71 = 70
revturn 85 = 86
revturn 86 = 85
revturn x  = x

invsol :: B.ByteString -> B.ByteString
{-# INLINE invsol #-}
invsol = B.reverse . B.map revturn

onesOr :: Array Int B.ByteString
onesOr = listArray (1, 5) ["T", "U", "V", "W", "H"]

twosOr :: Array Int B.ByteString
twosOr = listArray (1, 16) $! filter (not . hasPair) $! take 25 $! iterate next "TT"

thrsOr :: Array Int B.ByteString
thrsOr = listArray (1, 44) $! filter (not . hasPair) $! take 125 $! iterate next "TTT"

foursOr :: Array Int B.ByteString
{-# INLINE fours #-}
foursOr = listArray (1, 128) $! filter (not . hasPair) $! take 625 $! iterate next "TTTT"

fivesOr :: Array Int B.ByteString
{-# INLINE fives #-}
fivesOr = listArray (1, 392) $! filter (not . hasPair) $! take 59049 $! iterate next "TTTTT"

sixesOr :: Array Int B.ByteString
{-# INLINE sixes #-}
sixesOr = listArray (1, 1348) $! filter (not . hasPair) $! take 531441 $! iterate next "TTTTTT"


ones :: Array Int B.ByteString
ones = listArray (1, 9) ["R", "S", "T", "U", "V", "W", "F", "G", "H"]

twos :: Array Int B.ByteString
twos = listArray (1, 54) $! filter (not . hasPair) $! take 81 $! iterate next "RR"

thrs :: Array Int B.ByteString
thrs = listArray (1, 324) $! filter (not . hasPair) $! take 729 $! iterate next "RRR"

fours :: Array Int B.ByteString
{-# INLINE foursOr #-}
fours = listArray (1, 1944) $! filter (not . hasPair) $! take 6561 $! iterate next "RRRR"

fives :: Array Int B.ByteString
{-# INLINE fivesOr #-}
fives = listArray (1, 11664) $! filter (not . hasPair) $! take 59049 $! iterate next "RRRRR"

sixes :: Array Int B.ByteString
{-# INLINE sixesOr #-}
sixes = listArray (1, 69984) $! filter (not . hasPair) $! take 531441 $! iterate next "RRRRRR"

intct :: Cube -> Array Int B.ByteString -> Array Int B.ByteString -> IO ()
intct cube arr1 arr2 = do
    forM_ (indices arr1) $ \i -> do
        forM_ (indices arr2) $ \j -> do
            when ({-mplTurns cube (arr1 ! i) == mplTurns (initCube) (arr2 ! j)-}True) $ do
                --D.putStrLn . solShow $ B.append (arr1 ! i) ( invsol $ arr2 ! j)
                D.writeFile "./nineMoves.txt" $ solShow $ B.append (arr1 ! i) ( invsol $ arr2 ! j)
--or $ (flip map) (indices arr1) $ \x ->

solve0 :: Cube -> Array Int B.ByteString -> IO Bool
solve0 cube arr = fmap or $ forM (indices arr) $ \i -> do
    let cb = mplTurns cube (arr ! i)
        sv = solved cb
    when sv $ D.putStrLn (arr ! i)
    return sv

solve2 :: Cube -> Word8 -> Word8 -> IO ()
solve2 cube left right = do

    when (left  <= 1 && right >= 1) $ do
        putStrLn "1 move..."
        t1 <- solve0 cube ones
        when t1 $ do
            putStrLn "Cube is probably solved"
            exitSuccess
    
    when (left  <= 2 && right >= 2) $ do
        putStrLn "2 moves..."
        t2 <- solve0 cube twos
        when t2 $ do
            putStrLn "Cube is probably solved"
            exitSuccess

    when (left  <= 3 && right >= 3) $ do
        putStrLn "3 moves..."
        t3 <- solve0 cube thrs
        when t3 $ do
            putStrLn "Cube is probably solved"
            exitSuccess

    when (left  <= 4 && right >= 5) $ do
        putStrLn "4 moves..."
        t4 <- solve0 cube fours
        when t4 $ do
            putStrLn "Cube is probably solved"
            exitSuccess

    when (left  <= 5 && right >= 5) $ do
        putStrLn "5 moves..."
        t5 <- solve0 cube fives
        when t5 $ do
            putStrLn "Cube is probably solved"
            exitSuccess

    when (left  <= 6 && right >= 6) $ do
        putStrLn "6 moves..."
        t6 <- solve0 cube sixes
        when t6 $ do
            putStrLn "Cube is probably solved"
            exitSuccess

    when (left  <= 7 && right >= 7) $ do
        putStrLn "7 moves..."
        intct cube fours thrs

    when (left  <= 8 && right >= 8) $ do
        putStrLn "8 moves..."
        intct cube fours fours
    
    when (left  <= 9 && right >= 9) $ do
        putStrLn "9 moves..."
        intct cube sixes thrs
   
    when (left  <= 10 && right >= 10) $ do
        putStrLn "10+ moves..."
        intct cube fives fives

    when (left  <= 11 && right >= 11) $ do
        putStrLn "11+ moves..."
        intct cube sixes fives

    when (left  <= 12 && right >= 12) $ do
        putStrLn "12+ moves..."
        intct cube sixes sixes
    --intct cube sixes ones
    --F' R2 U' F U2 
    -- F' L2 U L y' L' U' L U L' U' L U F U' x' y2
    -- y' U F' U' L' U L U' L' U L y L' U' L2 F
    -- U R2 U2 R' F R2 U' F2 U' x | UTWSFTVHV
    -- 80 21 10 72 40 61 31 51
    -- RURUR
    -- URUR
    -- RUR
    -- UR
    {-
80 21 10 72 40 61 31 51
80 21
72 10
=====
40 61
51 31
VTWRFTVHV
UTWSFTVHV
FSVHTUGUF
UHRGRGRVFR
TGRWFUTVFR
FURVTVFVFS
VHUGSWHUGS
VHUFWHRVGS
FSVFRVGSHS
FURVTVFVFS
VHUGSWHUGS
VHUFWHRVGS
FSVFRVGSHS
    -}