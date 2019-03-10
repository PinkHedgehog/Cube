{-# LANGUAGE OverloadedStrings #-}
module Solve3x3 where

import  qualified Data.ByteString as B
import Data.Word
import Cube3x3
import Control.Monad (when, unless, forM_, forM)
import qualified Data.ByteString.Char8 as D (putStr, putStrLn, writeFile)
import System.Exit
import Algorithms (words')
import Data.Array.Unboxed
import qualified Data.Array as A
import Data.List (iterate, elem)

turnsAndRots :: A.Array Word8 (Cube -> Cube)
turnsAndRots = A.listArray (1, 36) [ turnR, turnR', turnR2, turnF, turnF', turnF2
                                  , turnU, turnU', turnU2, turnD, turnD', turnD2
                                  , turnL, turnL', turnL2, turnB, turnB', turnB2
                                  , turnE, turnE', turnE2, turnS, turnS', turnS2
                                  , turnM, turnM', turnM2, rotX, rotX', rotX2
                                  , rotY, rotY', rotY2, rotZ, rotZ', rotZ2
                                  ]
turnsIndicesOriented :: UArray Word8 Word8
turnsIndicesOriented = listArray (1, 10) [3, 6, 7, 8, 9, 10, 11, 12, 15, 18]

lToTurn :: Word8 -> Cube -> Cube
lToTurn x | x < 1 || x > 36 = id
          | otherwise = turnsAndRots ! x

mplTurns :: Cube -> B.ByteString -> Cube
{-# INLINE mplTurns #-}
mplTurns = B.foldl' (flip lToTurn)

nextChar :: Word8 -> Word8
nextChar x | x == 18 = 1
           | otherwise = x+1

nextQChar :: Word8 -> Word8
nextQChar x | elem x [1, 4, 7, 10, 13, 16] = x + 1
            | elem x [2, 5, 8, 11, 14] = x + 3
            | otherwise = 1

nextMChar :: Word8 -> Word8
nextMChar x | elem x [7, 8, 25, 26] = x + 1
            | x == 9 = 25
            | otherwise = 7

nextOrientedChar :: Word8 -> Word8
nextOrientedChar x | x `elem` [3, 9, 12, 15] = x + 3
                   | x `elem` [6..8] = x + 1
                   | x == 18 = 3

nextM :: B.ByteString -> B.ByteString
nextM bstring | B.all (== 27) bstring = odds !! (B.length bstring)
              | B.head bstring == 27 = nextMChar b `B.cons` (nextM $! a)
              | otherwise = B.cons (nextMChar b) a
    where
        (b, a) = fromMaybe $ B.uncons bstring
        fromMaybe Nothing  = (0, "")
        fromMaybe (Just x) = x
        len = B.length bstring
        odds = [ "\a", "\EM\a", "\a\EM\a", "\EM\a\EM\a", "\a\EM\a\EM\a"
               , "\EM\a\EM\a\EM\a", "\a\EM\a\EM\a\EM\a", "\EM\a\EM\a\EM\a\EM\a"
               , "\a\EM\a\EM\a\EM\a\EM\a", "\EM\a\EM\a\EM\a\EM\a\EM\a", "\a\EM\a\EM\a\EM\a\EM\a\EM\a"
               , "\EM\a\EM\a\EM\a\EM\a\EM\a\EM\a", "\a\EM\a\EM\a\EM\a\EM\a\EM\a\EM\a"
               , "\EM\a\EM\a\EM\a\EM\a\EM\a\EM\a\EM\a", "\a\EM\a\EM\a\EM\a\EM\a\EM\a\EM\a\EM\a"
               ]


nextOriented :: B.ByteString -> B.ByteString
nextOriented bstring | B.all (== 18) bstring = odds !! (B.length bstring)
                     | B.head bstring == 18 = nextOrientedChar b `B.cons` (nextOriented $! a)
                     | otherwise = B.cons (nextOrientedChar b) a
    where
        (b, a) = fromMaybe $ B.uncons bstring
        fromMaybe Nothing  = (0, "")
        fromMaybe (Just x) = x
        len = B.length bstring
        odds = [ "\ETX", "\a\ETX", "\ETX\a\ETX", "\a\ETX\a\ETX", "\ETX\a\ETX\a\ETX", "\a\ETX\a\ETX\a\ETX"
               , "\ETX\a\ETX\a\ETX\a\ETX", "\a\ETX\a\ETX\a\ETX\a\ETX", "\ETX\a\ETX\a\ETX\a\ETX\a\ETX"
               , "\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX", "\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX"
               , "\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX", "\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX"
               , "\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX", "\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX\a\ETX"
               ]

next :: B.ByteString -> B.ByteString
next bstring | B.length bstring >= 20 = undefined
             | B.all ( == 18) bstring = odds !! len
             | B.head bstring == 18 = nextChar b `B.cons` (next $! a)
             | otherwise = B.cons (nextChar b) a
    where
        (b, a) = fromMaybe $ B.uncons bstring
        fromMaybe Nothing  = (0, "")
        fromMaybe (Just x) = x
        len = B.length bstring
        odds = [ "\SOH", "\a\SOH", "\SOH\a\SOH", "\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH", "\a\SOH\a\SOH\a\SOH"
               , "\SOH\a\SOH\a\SOH\a\SOH", "\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               , "\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               , "\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               , "\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               ]

nextQ :: B.ByteString -> B.ByteString
nextQ bstring | B.length bstring >= 20 = undefined
             | B.all ( == 17) bstring = odds !! len
             | B.head bstring == 17 = nextQChar b `B.cons` (nextQ $! a)
             | otherwise = B.cons (nextQChar b) a
    where
        (b, a) = fromMaybe $ B.uncons bstring
        fromMaybe Nothing  = (0, "")
        fromMaybe (Just x) = x
        len = B.length bstring
        odds = [ "\SOH", "\a\SOH", "\SOH\a\SOH", "\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH", "\a\SOH\a\SOH\a\SOH"
               , "\SOH\a\SOH\a\SOH\a\SOH", "\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               , "\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               , "\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               , "\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH", "\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH\a\SOH"
               ]

hasPair :: B.ByteString -> Bool
hasPair str | B.length str < 2 = False
hasPair bstr = or $ map (\i->isRight (i,bstr) || isFrontal (i,bstr) || isUp (i,bstr) ||
    isLeft (i,bstr) || isBack (i,bstr) || isDown (i,bstr)) [0..(B.length bstr - 2)]
    where
        isRight (i, b) = (b ! i) >= 1 && (b ! i) <= 3 && (b ! (i+1)) >= 1 && (b ! (i+1)) <= 3
        isFrontal (i, b) = (b ! i) >= 4 && (b ! i) <= 6 && (b ! (i+1)) >= 4 && (b ! (i+1)) <= 6
        isUp (i, b) = (b ! i) >= 7 && (b ! i) <= 9 && (b ! (i+1)) >= 7 && (b ! (i+1)) <= 9
        isDown (i, b) = (b ! i) >= 10 && (b ! i) <= 12 && (b ! (i+1)) >= 10 && (b ! (i+1)) <= 12
        isLeft (i, b) = (b ! i) >= 13 && (b ! i) <= 15 && (b ! (i+1)) >= 13 && (b ! (i+1)) <= 15
        isBack (i, b) = (b ! i) >= 16 && (b ! i) <= 18 && (b ! (i+1)) >= 16 && (b ! (i+1)) <= 18
        isM    (i, b) = (b ! i) >= 25 && (b ! i) <= 27 && (b ! (i+1)) >= 25 && (b ! (i+1)) <= 27
        (!) = B.index

solShow :: B.ByteString -> B.ByteString
{-# INLINE solShow #-}
solShow = B.concatMap conv

conv :: Word8 -> B.ByteString
conv 1 = "R "
conv 2 = "R' "
conv 3 = "R2 "
conv 4 = "F "
conv 5 = "F' "
conv 6 = "F2 "
conv 7 = "U "
conv 8 = "U' "
conv 9 = "U2 "
conv 10 = "D "
conv 11 = "D' "
conv 12 = "D2 "
conv 13 = "L "
conv 14 = "L' "
conv 15 = "L2 "
conv 16 = "B "
conv 17 = "B' "
conv 18 = "B2 "
conv 19 = "E "
conv 20 = "E' "
conv 21 = "E2 "
conv 22 = "S "
conv 23 = "S' "
conv 24 = "S2 "
conv 25 = "M "
conv 26 = "M' "
conv 27 = "M2 "
conv 36 = "z2 "
conv 35 = "z' "
conv 34 = "z "
conv 33 = "y2 "
conv 32 = "y' "
conv 31 = "y "
conv 30 = "x2 "
conv 29 = "x' "
conv 28 = "x "

conv' :: B.ByteString -> Word8
conv' "R"  = 1
conv' "R'" = 2
conv' "R2" = 3
conv' "F"  = 4
conv' "F'" = 5
conv' "F2" = 6
conv' "U"  = 7
conv' "U'" = 8
conv' "U2" = 9
conv' "D"  = 10
conv' "D'" = 11
conv' "D2" = 12
conv' "L"  = 13
conv' "L'" = 14
conv' "L2" = 15
conv' "B"  = 16
conv' "B'" = 17
conv' "B2" = 18

rotToOriented :: Cube -> B.ByteString
rotToOriented (Cube arr) | arr ! 21 == 3 = case arr ! 23 of
    4 -> B.singleton 31
    5 -> ""
    6 -> B.singleton 32
    7 -> B.singleton 33
                         | arr ! 21 == 4 = 35 `B.cons` rotToOriented (rotZ' $ Cube arr)
                         | arr ! 21 == 5 = 29 `B.cons` rotToOriented (rotX' $ Cube arr)
                         | arr ! 21 == 6 = 34 `B.cons` rotToOriented (rotZ  $ Cube arr)
                         | arr ! 21 == 7 = 28 `B.cons` rotToOriented (rotX  $ Cube arr)
                         | arr ! 21 == 8 = 30 `B.cons` rotToOriented (rotX2 $ Cube arr)
                         | otherwise = ""


orient :: Cube -> Word8 -> B.ByteString -> IO ()
orient cube n bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (oriented cb) $ do
            when (solved cb) $ do
                D.putStr . solShow $ bstr
                D.putStr "solved"
            unless (solved cb) $ do
                permute cb n bstr "\ETX"
            --D.putStrLn ""
    when (B.length bstr < 19) $ do
        when (B.length bstr < B.length (next bstr)) $ do
            putStrLn . show $ (B.length bstr + 1)
        orient cube n (next bstr)

createBlock :: Cube -> B.ByteString -> IO ()
createBlock cube bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (hasBlocks cb) $ do
            D.putStr . solShow $ bstr
            when (solved cb) $ do
                D.putStr "solved"
            D.putStrLn ""
    --when (B.length bstr < 19) $ do
    --    when (B.length bstr < B.length (next bstr)) $ do
    --        putStrLn . show $ (B.length bstr + 1)
    createBlock cube (nextQ bstr)
--"42 31 51 20 82 10 61 72 190 171 181 101 130 150 160 140 120 111 200 90 3 4 5 6 7 8"
epll :: Cube -> B.ByteString -> IO ()
epll cube bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (solved cb) $ do
            D.putStrLn . solShow $ bstr
            D.putStrLn "solved"
    when (B.length bstr < 11) $ do
        when (B.length bstr < B.length (nextM bstr)) $ do
            putStrLn . show $ (B.length bstr + 1)
        epll cube(nextM bstr)

permute :: Cube -> Word8 -> B.ByteString -> B.ByteString -> IO ()
permute cube n prev bstr = do
    unless (hasPair bstr) $ do
        let cb = mplTurns cube bstr
        when (solved cb) $ do
            D.putStr . solShow $ prev
            D.putStr . solShow $ bstr
            D.putStrLn "solved"
    when (B.length bstr < fromIntegral n) $ permute cube n prev (nextOriented bstr)



getCubeN :: String -> Cube
{-# INLINE getCubeN #-}
getCubeN = Cube . listArray (1, 26) . (map read) . words

getCubeS :: B.ByteString -> Cube
{-# INLINE getCubeS #-}
getCubeS = mplTurns initCube . B.pack . (map conv') . words'
