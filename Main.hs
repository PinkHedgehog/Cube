{-# LANGUAGE OverloadedStrings #-}
module Main where

import Solve
import System.Exit
import Cube
import System.Environment (getArgs)
import qualified Data.ByteString as B
import Data.Array.Unboxed
import Data.Word (Word8)
import Control.Monad (when, unless)

main = do
    s <- fmap getCube getLine
    when (not . checkCube $ s) $ do
        die "Wrong cube!"
    args <- fmap (map read) getArgs
    --let t = Cube $ listArray (1, 8) $ case (length s) of
    --        8 -> s
    --        _ -> [80, 21, 10, 72, 40, 61, 31, 51]
    when (null args || head args == 0) $ do
        putStr . show $ s
        putStrLn ""
        orient s 5 "R"
    when (length args > 1) $ do
            --let odds = ["", "R", "UR", "RUR", "URUR", "RURUR", "URURUR", "RURURUR", "URURURUR", "RURURURUR", "URURURURUR", "RURURURURUR"]
             --   bstr = odds !! (args !! 1 :: Int)
        orient s (fromIntegral $ args !! 1) "R"
    {-
    unless (null args || head args == 0) $ do
        putStr . show $ s
        putStrLn ""
        if (length args > 2) 
            then 
                --solve2 s (fromIntegral $ args !! 1) (fromIntegral $ args !! 2)
                intct s sixes thrs
            else
                solve2 s 1 6
    -}
        putStrLn "kek"
