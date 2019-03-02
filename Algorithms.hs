module Algorithms where

import Data.Array.Unboxed
import Data.Word
import qualified Data.ByteString as B

binSearch :: UArray Word8 Word8 -> Word8 -> Bool
binSearch arr x = binSearch' arr x l r
    where (l, r) = bounds arr

binSearch' :: UArray Word8 Word8 -> Word8 -> Word8 -> Word8-> Bool
binSearch' arr x l r | r == l + 1 = x == arr ! r
                     | x <  arr ! m = binSearch' arr x l m
                     | x >= arr ! m = binSearch' arr x m r
    where m = div (l + r) 2

words' :: B.ByteString -> [B.ByteString]
words' str | str == "" = []
           | B.head str == 32 = words' . B.tail $ str
           | otherwise = B.takeWhile (/= 32) str : (words' $ B.dropWhile (/= 32) str)

--main = putStrLn "Hello World"