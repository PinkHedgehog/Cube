module Cube3x3 where

import Data.Array.Unboxed
import Data.Word
import Data.List (sort)
import Data.Char (toLower)
import qualified Data.ByteString as B
newtype Cube = Cube (UArray Word8 Word8) deriving (Eq, Show)

initCube :: Cube
initCube = Cube $ listArray (1, 26) $ [10 * i | i <- [1..20]] ++ [3, 4, 5, 6, 7, 8]
-- U L F R B D


spinA :: Word8 -> Word8
{-# INLINE spinA #-}
spinA x = x - mod x 10 + mod (mod x 10 + 1) 3

spinB :: Word8 -> Word8
{-# INLINE spinB #-}
spinB x = x - mod x 10 + mod (mod x 10 + 2) 3

spinE :: Word8 -> Word8
{-# INLINE spinE #-}
spinE x = if (odd x) then (x-1) else (x + 1)

getSide :: Char -> Cube -> [Word8]
getSide c cube | c' == 'u' = g cube u
               | c' == 'l' = g (rotX . rotY' $ cube) u
               | c' == 'f' = g (rotX $ cube) u
               | c' == 'r' = g (rotX . rotY $ cube) u
               | c' == 'b' = g (rotX . rotY2 $ cube) u
               | c' == 'd' = g (rotX2 $ cube) u
    where
        c' = toLower c

        u = [1, 12, 2, 11, 21, 9, 4, 10, 3]
        
        g :: Cube -> [Word8] -> [Word8]
        {-# INLINE g #-}
        g (Cube ay) = map (\x -> ay ! x)

showUpSide :: Cube -> String
showUpSide (Cube arr) = show (arr ! 1) ++ " " ++ show (arr ! 12) ++ " " ++ show (arr ! 2) ++ "\n" ++
    show (arr ! 11) ++ " " ++ show (arr ! 21) ++ " " ++ show (arr ! 9) ++ "\n" ++
        show (arr ! 4) ++ " " ++ show (arr ! 10) ++ " " ++ show (arr ! 3) ++ "\n"
{-
#цитаты
8.02.19
Тарасов:
Я не видел ни одного человека, который видел ДКА хотя бы на улице
-}
{-
           1 12  2
        / 11     9 \
           4 10  3

     16   15    14   13

          50 18 60
        \ 19    17 / 
          80 20 70

-}

turnR' :: Cube -> Cube
turnR' (Cube arr) = Cube $! (//) arr [ (2 :: Word8, spinA (arr ! 7))
                                     , (3 :: Word8, spinB (arr ! 2))
                                     , (6 :: Word8, spinA (arr ! 3))
                                     , (7 :: Word8, spinB (arr ! 6))
                                     , (9 :: Word8, arr ! 13)
                                     , (14 :: Word8, arr ! 9)
                                     , (17 :: Word8, arr ! 14)
                                     , (13 :: Word8, arr ! 17)
                                     ]

turnR :: Cube -> Cube
turnR (Cube arr) = Cube $! (//) arr [ (2 :: Word8, spinA (arr ! 3))
                                    , (3 :: Word8, spinB (arr ! 6))
                                    , (6 :: Word8, spinA (arr ! 7))
                                    , (7 :: Word8, spinB (arr ! 2))
                                    , (9 :: Word8, arr ! 14)
                                    , (14 :: Word8, arr ! 17)
                                    , (17 :: Word8, arr ! 13)
                                    , (13 :: Word8, arr ! 9)
                                    ]

turnR2 :: Cube -> Cube
turnR2 (Cube arr) = Cube $! (//) arr [ (2 :: Word8, arr ! 6)
                                     , (3 :: Word8, arr ! 7)
                                     , (6 :: Word8, arr ! 2)
                                     , (7 :: Word8, arr ! 3)
                                     , (9 :: Word8, arr ! 17)
                                     , (14 :: Word8, arr ! 13)
                                     , (13 :: Word8, arr ! 14)
                                     , (17 :: Word8, arr ! 9)
                                     ]

turnL' :: Cube -> Cube
turnL' (Cube arr) = Cube $! (//) arr [ (1 :: Word8, spinB (arr ! 4))
                                     , (4 :: Word8, spinA (arr ! 5))
                                     , (5 :: Word8, spinB (arr ! 8))
                                     , (8 :: Word8, spinA (arr ! 1))
                                     , (11 :: Word8, arr ! 15)
                                     , (16 :: Word8, arr ! 11)
                                     , (19 :: Word8, arr ! 16)
                                     , (15 :: Word8, arr ! 19)
                                     ]

turnL :: Cube -> Cube
turnL (Cube arr) = Cube $! (//) arr [ (1 :: Word8, spinB (arr ! 8))
                                    , (4 :: Word8, spinA (arr ! 1))
                                    , (5 :: Word8, spinB (arr ! 4))
                                    , (8 :: Word8, spinA (arr ! 5))
                                    , (11 :: Word8, arr ! 16)
                                    , (16 :: Word8, arr ! 19)
                                    , (19 :: Word8, arr ! 15)
                                    , (15 :: Word8, arr ! 11)
                                    ]

turnL2 :: Cube -> Cube
turnL2 (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 5)
                                     , (4 :: Word8, arr ! 8)
                                     , (5 :: Word8, arr ! 1)
                                     , (8 :: Word8, arr ! 4)
                                     , (11 :: Word8, arr ! 19)
                                     , (16 :: Word8, arr ! 15)
                                     , (19 :: Word8, arr ! 11)
                                     , (15 :: Word8, arr ! 16)
                                     ]


turnF :: Cube -> Cube
turnF (Cube arr) = Cube $! (//) arr [ (3 :: Word8, spinA (arr ! 4))
                                    , (4 :: Word8, spinB (arr ! 5))
                                    , (5 :: Word8, spinA (arr ! 6))
                                    , (6 :: Word8, spinB (arr ! 3))
                                    , (10 :: Word8, spinE (arr ! 15))
                                    , (15 :: Word8, spinE (arr ! 18))
                                    , (18 :: Word8, spinE (arr ! 14))
                                    , (14 :: Word8, spinE (arr ! 10))
                                    ]

turnF' :: Cube -> Cube
turnF' (Cube arr) = Cube $! (//) arr [ (3 :: Word8, spinA (arr ! 6))
                                     , (4 :: Word8, spinB (arr ! 3))
                                     , (5 :: Word8, spinA (arr ! 4))
                                     , (6 :: Word8, spinB (arr ! 5))
                                     , (10 :: Word8, spinE (arr ! 14))
                                     , (15 :: Word8, spinE (arr ! 10))
                                     , (18 :: Word8, spinE (arr ! 15))
                                     , (14 :: Word8, spinE (arr ! 18))
                                     ]

turnF2 :: Cube -> Cube
turnF2 (Cube arr) = Cube $! (//) arr [ (3 :: Word8, arr ! 5)
                                     , (4 :: Word8, arr ! 6)
                                     , (5 :: Word8, arr ! 3)
                                     , (6 :: Word8, arr ! 4)
                                     , (10 :: Word8, (arr ! 18))
                                     , (15 :: Word8, (arr ! 14))
                                     , (14 :: Word8, (arr ! 15))
                                     , (18 :: Word8, (arr ! 10))
                                     ]

turnB :: Cube -> Cube
turnB (Cube arr) = Cube $! (//) arr [ (1 :: Word8, spinA (arr ! 2))
                                    , (2 :: Word8, spinB (arr ! 7))
                                    , (7 :: Word8, spinA (arr ! 8))
                                    , (8 :: Word8, spinB (arr ! 1))
                                    , (12 :: Word8, spinE (arr ! 13))
                                    , (13 :: Word8, spinE (arr ! 20))
                                    , (20 :: Word8, spinE (arr ! 16))
                                    , (16 :: Word8, spinE (arr ! 12))
                                    ]

turnB' :: Cube -> Cube
turnB' (Cube arr) = Cube $! (//) arr [ (1 :: Word8, spinA (arr ! 8))
                                     , (2 :: Word8, spinB (arr ! 1))
                                     , (7 :: Word8, spinA (arr ! 2))
                                     , (8 :: Word8, spinB (arr ! 7))
                                     , (12 :: Word8, spinE (arr ! 16))
                                     , (13 :: Word8, spinE (arr ! 12))
                                     , (20 :: Word8, spinE (arr ! 13))
                                     , (16 :: Word8, spinE (arr ! 20))
                                     ]

turnB2 :: Cube -> Cube
turnB2 (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 7)
                                     , (2 :: Word8, arr ! 8)
                                     , (7 :: Word8, arr ! 1)
                                     , (8 :: Word8, arr ! 2)
                                     , (12 :: Word8, (arr ! 20))
                                     , (13 :: Word8, (arr ! 16))
                                     , (20 :: Word8, (arr ! 12))
                                     , (16 :: Word8, (arr ! 13))
                                     ]


turnU :: Cube -> Cube
turnU (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 4)
                                    , (2 :: Word8, arr ! 1)
                                    , (3 :: Word8, arr ! 2)
                                    , (4 :: Word8, arr ! 3)
                                    , (10 :: Word8, arr ! 9)
                                    , (11 :: Word8, arr ! 10)
                                    , (12 :: Word8, arr ! 11)
                                    , (9 :: Word8, arr ! 12)
                                    ]

turnU' :: Cube -> Cube
turnU' (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 2)
                                     , (2 :: Word8, arr ! 3)
                                     , (3 :: Word8, arr ! 4)
                                     , (4 :: Word8, arr ! 1)
                                     , (10 :: Word8, (arr ! 11))
                                     , (11 :: Word8, (arr ! 12))
                                     , (12 :: Word8, (arr ! 9))
                                     , (9 :: Word8, (arr ! 10))
                                     ]


turnU2 :: Cube -> Cube
turnU2 (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 3)
                                     , (2 :: Word8, arr ! 4)
                                     , (3 :: Word8, arr ! 1)
                                     , (4 :: Word8, arr ! 2)
                                     , (10 :: Word8, (arr ! 12))
                                     , (12 :: Word8, (arr ! 10))
                                     , (9 :: Word8, (arr ! 11))
                                     , (11 :: Word8, (arr ! 9))
                                     ]

turnD :: Cube -> Cube
turnD (Cube arr) = Cube $! (//) arr [ (5 :: Word8, arr ! 8)
                                    , (6 :: Word8, arr ! 5)
                                    , (7 :: Word8, arr ! 6)
                                    , (8 :: Word8, arr ! 7)
                                    , (17 :: Word8, arr ! 18)
                                    , (18 :: Word8, arr ! 19)
                                    , (19 :: Word8, arr ! 20)
                                    , (20 :: Word8, arr ! 17)
                                    ]

turnD' :: Cube -> Cube
turnD' (Cube arr) = Cube $! (//) arr [ (5 :: Word8, arr ! 6)
                                     , (6 :: Word8, arr ! 7)
                                     , (7 :: Word8, arr ! 8)
                                     , (8 :: Word8, arr ! 5)
                                     , (17 :: Word8, (arr ! 20))
                                     , (18 :: Word8, (arr ! 17))
                                     , (19 :: Word8, (arr ! 18))
                                     , (20 :: Word8, (arr ! 19))
                                     ]


turnD2 :: Cube -> Cube
turnD2 (Cube arr) = Cube $! (//) arr [ (5 :: Word8, arr ! 7)
                                     , (6 :: Word8, arr ! 8)
                                     , (7 :: Word8, arr ! 5)
                                     , (8 :: Word8, arr ! 6)
                                     , (17 :: Word8, (arr ! 19))
                                     , (18 :: Word8, (arr ! 20))
                                     , (19 :: Word8, (arr ! 17))
                                     , (20 :: Word8, (arr ! 18))
                                     ]

-- u  l  f  r  b  d
-- 21 22 23 24 25 26
turnM :: Cube -> Cube
turnM (Cube arr) = Cube $! (//) arr [ (21 :: Word8, arr ! 25)
                                    , (23 :: Word8, arr ! 21)
                                    , (25 :: Word8, arr ! 26)
                                    , (26 :: Word8, arr ! 23)
                                    , (10 :: Word8, spinE (arr ! 12))
                                    , (12 :: Word8, spinE (arr ! 20))
                                    , (20 :: Word8, spinE (arr ! 18))
                                    , (18 :: Word8, spinE (arr ! 10))
                                    ]

turnM' :: Cube -> Cube
turnM' (Cube arr) = Cube $! (//) arr [ (21 :: Word8, arr ! 23)
                                     , (23 :: Word8, arr ! 26)
                                     , (25 :: Word8, arr ! 21)
                                     , (26 :: Word8, arr ! 25)
                                     , (10 :: Word8, spinE (arr ! 18))
                                     , (12 :: Word8, spinE (arr ! 10))
                                     , (20 :: Word8, spinE (arr ! 12))
                                     , (18 :: Word8, spinE (arr ! 20))
                                     ]
-- F [R U R' U'] F'
-- F [U R U' R'] F'
turnM2 :: Cube -> Cube
turnM2 (Cube arr) = Cube $! (//) arr [ (21 :: Word8, arr ! 26)
                                     , (23 :: Word8, arr ! 25)
                                     , (25 :: Word8, arr ! 23)
                                     , (26 :: Word8, arr ! 21)
                                     , (10 :: Word8, arr ! 20)
                                     , (12 :: Word8, arr ! 18)
                                     , (20 :: Word8, arr ! 10)
                                     , (18 :: Word8, arr ! 12)
                                     ]

turnE :: Cube -> Cube
turnE (Cube arr) = Cube $! (//) arr [ (22 :: Word8, arr ! 25)
                                    , (23 :: Word8, arr ! 22)
                                    , (24 :: Word8, arr ! 23)
                                    , (25 :: Word8, arr ! 24)
                                    , (13 :: Word8, spinE (arr ! 14))
                                    , (14 :: Word8, spinE (arr ! 15))
                                    , (15 :: Word8, spinE (arr ! 16))
                                    , (16 :: Word8, spinE (arr ! 13))
                                    ]

turnE' :: Cube -> Cube
turnE' (Cube arr) = Cube $! (//) arr [ (22 :: Word8, arr ! 23)
                                     , (23 :: Word8, arr ! 24)
                                     , (24 :: Word8, arr ! 25)
                                     , (25 :: Word8, arr ! 22)
                                     , (13 :: Word8, spinE (arr ! 16))
                                     , (14 :: Word8, spinE (arr ! 13))
                                     , (15 :: Word8, spinE (arr ! 14))
                                     , (16 :: Word8, spinE (arr ! 15))
                                     ]

turnE2 :: Cube -> Cube
turnE2 (Cube arr) = Cube $! (//) arr [ (22 :: Word8, arr ! 24)
                                     , (23 :: Word8, arr ! 25)
                                     , (24 :: Word8, arr ! 22)
                                     , (25 :: Word8, arr ! 23)
                                     , (13 :: Word8, arr ! 15)
                                     , (14 :: Word8, arr ! 16)
                                     , (15 :: Word8, arr ! 13)
                                     , (16 :: Word8, arr ! 14)
                                     ]

turnS :: Cube -> Cube
turnS (Cube arr) = Cube $! (//) arr [ (21 :: Word8, arr ! 22)
                                    , (22 :: Word8, arr ! 26)
                                    , (24 :: Word8, arr ! 21)
                                    , (26 :: Word8, arr ! 24)
                                    , (9  :: Word8, spinE (arr ! 11))
                                    , (11 :: Word8, spinE (arr ! 19))
                                    , (19 :: Word8, spinE (arr ! 17))
                                    , (17 :: Word8, spinE (arr ! 9))
                                    ]

turnS' :: Cube -> Cube
turnS' (Cube arr) = Cube $! (//) arr [ (21 :: Word8, arr ! 24)
                                     , (22 :: Word8, arr ! 21)
                                     , (24 :: Word8, arr ! 26)
                                     , (26 :: Word8, arr ! 22)
                                     , (9  :: Word8, spinE (arr ! 17))
                                     , (11 :: Word8, spinE (arr ! 9))
                                     , (19 :: Word8, spinE (arr ! 11))
                                     , (17 :: Word8, spinE (arr ! 19))
                                     ]



turnS2 :: Cube -> Cube
turnS2 (Cube arr) = Cube $! (//) arr [ (21 :: Word8, arr ! 26)
                                     , (22 :: Word8, arr ! 24)
                                     , (24 :: Word8, arr ! 22)
                                     , (26 :: Word8, arr ! 21)
                                     , (9  :: Word8, arr ! 19)
                                     , (11 :: Word8, arr ! 17)
                                     , (19 :: Word8, arr ! 9)
                                     , (17 :: Word8, arr ! 11)
                                     ]



rotX :: Cube -> Cube
{-# INLINE rotX #-}
rotX = turnR . turnL' . turnM'

rotX' :: Cube -> Cube
{-# INLINE rotX' #-}
rotX' = turnR' . turnL . turnM

rotX2 :: Cube -> Cube
{-# INLINE rotX2 #-}
rotX2 = turnR2 . turnL2 . turnM2

rotY :: Cube -> Cube
{-# INLINE rotY #-}
rotY = turnU . turnD' . turnE'

rotY' :: Cube -> Cube
{-# INLINE rotY' #-}
rotY' = turnU' . turnD . turnE

rotY2 :: Cube -> Cube
{-# INLINE rotY2 #-}
rotY2 = turnU2 . turnD2 . turnE2

rotZ :: Cube -> Cube
{-# INLINE rotZ #-}
rotZ = turnF . turnB' . turnS

rotZ' :: Cube -> Cube
{-# INLINE rotZ' #-}
rotZ' = turnF' . turnB . turnS'

rotZ2 :: Cube -> Cube
{-# INLINE rotZ2 #-}
rotZ2 = turnF2 . turnB2 . turnS2

midIndices :: Cube -> [Word8]
midIndices (Cube arr) = map (\i -> arr ! i) [13, 14, 15, 16, 22, 23, 24, 25]

upIndices :: Cube -> [Word8]
upIndices (Cube arr) = map (\i -> arr ! i) [9, 10, 11, 12, 1, 2, 3, 4, 21]

downIndices :: Cube -> [Word8]
downIndices (Cube arr) = map (\i -> arr ! i) [5, 6, 7, 8, 17, 18, 19, 20, 26]

solved :: Cube -> Bool
solved cube = (not . null) $ filter (== initCube) $ map (\x -> x $ cube) $
    [ id, rotY, rotY', rotY2
    , rotX, rotY . rotX, rotY2 . rotX, rotY' . rotX
    , rotX', rotY . rotX', rotY2 . rotX', rotY' . rotX'
    , rotX2, rotY . rotX2, rotY2 . rotX2, rotY' . rotX2
    , rotZ, rotY . rotZ, rotY2 . rotZ, rotY' . rotZ
    , rotZ', rotY . rotZ', rotY2 . rotZ', rotY' . rotZ'
    ]

upCentre :: Word8
upCentre = 3

leftCentre :: Word8
leftCentre = 4

frontCentre :: Word8
frontCentre = 5

rightCentre :: Word8
rightCentre = 6

backCentre :: Word8
backCentre = 7

downCentre :: Word8
downCentre = 8

upPiece :: Word8 -> Bool
{-# INLINE upPiece #-}
upPiece = flip elem [10, 120, 20, 110, 3, 90, 40, 100, 30]



downPiece :: Word8 -> Bool
{-# INLINE downPiece #-}
downPiece = flip elem [50, 60, 70, 80, 170, 180, 190, 200, 8]

leftPiece :: Word8 -> Bool
{-# INLINE leftPiece #-}
leftPiece = flip elem [ 81, 161, 12
                      , 191, 4, 111
                      , 52, 151, 41
                      ]

rightPiece :: Word8 -> Bool
{-# INLINE rightPiece #-}
rightPiece = flip elem [ 21, 131, 72
                       , 91, 6, 171
                       , 32, 141, 61
                       ]

frontPiece :: Word8 -> Bool
{-# INLINE frontPiece #-}
frontPiece = flip elem [31, 62, 51, 42, 5, 101, 181, 150, 140]

frontPiece' :: Word8 -> Bool
{-# INLINE frontPiece' #-}
frontPiece' = flip elem [130, 140, 150, 160, 4, 5, 6, 7]


backPiece :: Word8 -> Bool
{-# INLINE backPiece #-}
backPiece = flip elem [82, 22, 71, 11, 201, 130, 121, 160, 7]


oriented :: Cube -> Bool
{-# INLINE oriented #-}
oriented cube = and (map (\x -> downPiece x || upPiece x) (upIndices cube ++ downIndices cube) ++ 
    map (\x -> frontPiece' x) (midIndices cube))


checkCube :: Cube -> Bool
checkCube (Cube c) = poss && oris
    where
        poss = [1, 2..20] == (map (\x -> div x 10) . sort . (take 20) . elems $ c)
        oris = 0 == (flip mod 3 $ sum . map (\x -> mod x 10) . (take 20) . elems $ c)

hasLeftBlock :: Cube -> Bool
{-# INLINE hasLeftBlock #-}
hasLeftBlock (Cube c) = and $ map (\i -> c ! i == 10 * i) [5, 8, 15, 16, 19]

hasRightBlock :: Cube -> Bool
{-# INLINE hasRightBlock #-}
hasRightBlock (Cube c) = and $ map (\i -> c ! i == 10 * i) [6, 7, 13, 14, 17]

hasBlocks :: Cube -> Bool
{-# INLINE hasBlocks #-}
hasBlocks cube = (hasRightBlock cube) && (hasLeftBlock cube)