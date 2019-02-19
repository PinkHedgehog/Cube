module Cube where

import Data.Array.Unboxed
import Data.Word
import Data.List (sort)
newtype Cube = Cube (UArray Word8 Word8) deriving (Eq)

initCube :: Cube
initCube = Cube $ listArray (1, 8) [10 * i | i <- [1..8]]

instance Show Cube where
    show (Cube arr) = show (arr ! 1) ++ " " ++ show (arr ! 2) ++ "\n" ++ show (arr ! 4) ++ " " ++ show (arr ! 3) ++
        "\n=====\n" ++ show (arr ! 5) ++  " " ++ show (arr ! 6) ++ "\n" ++ show (arr ! 8) ++ " " ++ show (arr ! 7)

spinA :: Word8 -> Word8
{-# INLINE spinA #-}
spinA x = x - mod x 10 + mod (mod x 10 + 1) 3

spinB :: Word8 -> Word8
{-# INLINE spinB #-}
spinB x = x - mod x 10 + mod (mod x 10 + 2) 3

turnS :: Cube -> Cube
turnS (Cube arr) = Cube $! (//) arr [ (2 :: Word8, spinA (arr ! 7))
                                   , (3 :: Word8, spinB (arr ! 2))
                                   , (6 :: Word8, spinA (arr ! 3))
                                   , (7 :: Word8, spinB (arr ! 6))
                                   ]

turnR :: Cube -> Cube
turnR (Cube arr) = Cube $! (//) arr [ (2 :: Word8, spinA (arr ! 3))
                                   , (3 :: Word8, spinB (arr ! 6))
                                   , (6 :: Word8, spinA (arr ! 7))
                                   , (7 :: Word8, spinB (arr ! 2))
                                   ]
{-
#цитаты
8.02.19
Тарасов:
Я не видел ни одного человека, который видел ДКА хотя бы на улице
-}

turnT :: Cube -> Cube
turnT (Cube arr) = Cube $! (//) arr [ (2 :: Word8, arr ! 6)
                                    , (3 :: Word8, arr ! 7)
                                    , (6 :: Word8, arr ! 2)
                                    , (7 :: Word8, arr ! 3)
                                    ]

turnF :: Cube -> Cube
turnF (Cube arr) = Cube $! (//) arr [ (3 :: Word8, spinA (arr ! 4))
                                    , (4 :: Word8, spinB (arr ! 5))
                                    , (5 :: Word8, spinA (arr ! 6))
                                    , (6 :: Word8, spinB (arr ! 3))
                                    ]

turnG :: Cube -> Cube
turnG (Cube arr) = Cube $! (//) arr [ (3 :: Word8, spinA (arr ! 6))
                                    , (4 :: Word8, spinB (arr ! 3))
                                    , (5 :: Word8, spinA (arr ! 4))
                                    , (6 :: Word8, spinB (arr ! 5))
                                    ]

turnH :: Cube -> Cube
turnH (Cube arr) = Cube $! (//) arr [ (3 :: Word8, arr ! 5)
                                    , (4 :: Word8, arr ! 6)
                                    , (5 :: Word8, arr ! 3)
                                    , (6 :: Word8, arr ! 4)
                                    ]

turnU :: Cube -> Cube
turnU (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 4)
                                    , (2 :: Word8, arr ! 1)
                                    , (3 :: Word8, arr ! 2)
                                    , (4 :: Word8, arr ! 3)
                                    ]

turnV :: Cube -> Cube
turnV (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 2)
                                    , (2 :: Word8, arr ! 3)
                                    , (3 :: Word8, arr ! 4)
                                    , (4 :: Word8, arr ! 1)
                                    ]


turnW :: Cube -> Cube
turnW (Cube arr) = Cube $! (//) arr [ (1 :: Word8, arr ! 3)
                                    , (2 :: Word8, arr ! 4)
                                    , (3 :: Word8, arr ! 1)
                                    , (4 :: Word8, arr ! 2)
                                    ]

rotX :: Cube -> Cube
rotX (Cube arr) = Cube $ (//) arr [ (1 :: Word8, spinB (arr ! 4))
                                  , (2 :: Word8, spinA (arr ! 3))
                                  , (3 :: Word8, spinB (arr ! 6))
                                  , (4 :: Word8, spinA (arr ! 5))
                                  , (5 :: Word8, spinB (arr ! 8))
                                  , (6 :: Word8, spinA (arr ! 7))
                                  , (7 :: Word8, spinB (arr ! 2))
                                  , (8 :: Word8, spinA (arr ! 1))
                                  ]

rotX' :: Cube -> Cube
rotX' (Cube arr) = Cube $ (//) arr [ (1 :: Word8, spinB (arr ! 8))
                                   , (2 :: Word8, spinA (arr ! 7))
                                   , (3 :: Word8, spinB (arr ! 2))
                                   , (4 :: Word8, spinA (arr ! 1))
                                   , (5 :: Word8, spinB (arr ! 4))
                                   , (6 :: Word8, spinA (arr ! 3))
                                   , (7 :: Word8, spinB (arr ! 6))
                                   , (8 :: Word8, spinA (arr ! 5))
                                   ]

rotX2 :: Cube -> Cube
rotX2 (Cube arr) = Cube $ (//) arr [ (1 :: Word8, arr ! 5)
                                   , (2 :: Word8, arr ! 6)
                                   , (3 :: Word8, arr ! 7)
                                   , (4 :: Word8, arr ! 8)
                                   , (5 :: Word8, arr ! 1)
                                   , (6 :: Word8, arr ! 2)
                                   , (7 :: Word8, arr ! 3)
                                   , (8 :: Word8, arr ! 4)
                                   ]

rotZ :: Cube -> Cube
rotZ (Cube arr) = Cube $ (//) arr [ (1 :: Word8, spinA (arr ! 8))
                                  , (2 :: Word8, spinB (arr ! 1))
                                  , (3 :: Word8, spinA (arr ! 4))
                                  , (4 :: Word8, spinB (arr ! 5))
                                  , (5 :: Word8, spinA (arr ! 6))
                                  , (6 :: Word8, spinB (arr ! 3))
                                  , (7 :: Word8, spinA (arr ! 2))
                                  , (8 :: Word8, spinB (arr ! 7))
                                  ]

rotZ' :: Cube -> Cube
rotZ' (Cube arr) = Cube $ (//) arr [ (1 :: Word8, spinA (arr ! 2))
                                   , (2 :: Word8, spinB (arr ! 7))
                                   , (3 :: Word8, spinA (arr ! 6))
                                   , (4 :: Word8, spinB (arr ! 3))
                                   , (5 :: Word8, spinA (arr ! 4))
                                   , (6 :: Word8, spinB (arr ! 5))
                                   , (7 :: Word8, spinA (arr ! 8))
                                   , (8 :: Word8, spinB (arr ! 1))
                                   ]

rotZ2 :: Cube -> Cube
rotZ2 (Cube arr) = Cube $ (//) arr [ (1 :: Word8, arr ! 7)
                                   , (2 :: Word8, arr ! 8)
                                   , (3 :: Word8, arr ! 5)
                                   , (4 :: Word8, arr ! 6)
                                   , (5 :: Word8, arr ! 3)
                                   , (6 :: Word8, arr ! 4)
                                   , (7 :: Word8, arr ! 1)
                                   , (8 :: Word8, arr ! 2)
                                   ]
rotY :: Cube -> Cube
rotY  (Cube arr) = Cube $ (//) arr [ (1 :: Word8, arr ! 4)
                                   , (2 :: Word8, arr ! 1)
                                   , (3 :: Word8, arr ! 2)
                                   , (4 :: Word8, arr ! 3)
                                   , (5 :: Word8, arr ! 6)
                                   , (6 :: Word8, arr ! 7)
                                   , (7 :: Word8, arr ! 8)
                                   , (8 :: Word8, arr ! 5)
                                   ]


rotY' :: Cube -> Cube
rotY' (Cube arr) = Cube $ (//) arr [ (1 :: Word8, arr ! 2)
                                   , (2 :: Word8, arr ! 3)
                                   , (3 :: Word8, arr ! 4)
                                   , (4 :: Word8, arr ! 1)
                                   , (5 :: Word8, arr ! 8)
                                   , (6 :: Word8, arr ! 5)
                                   , (7 :: Word8, arr ! 6)
                                   , (8 :: Word8, arr ! 7)
                                   ]


rotY2 :: Cube -> Cube
rotY2 (Cube arr) = Cube $ (//) arr [ (1 :: Word8, arr ! 3)
                                   , (2 :: Word8, arr ! 4)
                                   , (3 :: Word8, arr ! 1)
                                   , (4 :: Word8, arr ! 2)
                                   , (5 :: Word8, arr ! 7)
                                   , (6 :: Word8, arr ! 8)
                                   , (7 :: Word8, arr ! 5)
                                   , (8 :: Word8, arr ! 6)
                                   ]

solved :: Cube -> Bool
solved cube = (not . null) $ filter (== initCube) $ map (\x -> x $ cube) $
    [ id, rotY, rotY', rotY2
    , rotX, rotY . rotX, rotY2 . rotX, rotY' . rotX
    , rotX', rotY . rotX', rotY2 . rotX', rotY' . rotX'
    , rotX2, rotY . rotX2, rotY2 . rotX2, rotY' . rotX2
    , rotZ, rotY . rotZ, rotY2 . rotZ, rotY' . rotZ
    , rotZ', rotY . rotZ', rotY2 . rotZ', rotY' . rotZ'
    ]


upPiece :: Word8 -> Bool
{-# INLINE upPiece #-}
upPiece = flip elem [10, 20, 30, 40]

downPiece :: Word8 -> Bool
{-# INLINE downPiece #-}
downPiece = flip elem [50, 60, 70, 80]

leftPiece :: Word8 -> Bool
{-# INLINE leftPiece #-}
leftPiece = flip elem [81, 12, 52, 41]

rightPiece :: Word8 -> Bool
{-# INLINE rightPiece #-}
rightPiece = flip elem [21, 61, 32, 72]

frontPiece :: Word8 -> Bool
{-# INLINE frontPiece #-}
frontPiece = flip elem [31, 62, 51, 42]

backPiece :: Word8 -> Bool
{-# INLINE backPiece #-}
backPiece = flip elem [82, 22, 71, 11]

{-
upSide :: Cube -> Bool
{-# INLINE upSide #-}
upSide (Cube arr) = (arr ! 1) <= 43 && (arr ! 2) <= 43 && (arr ! 3) <= 43 && (arr ! 4) <= 43

downSide :: Cube -> Bool
{-# INLINE downSide #-}
downSide (Cube arr) = (arr ! 1) > 43 && (arr ! 2) > 43 && (arr ! 3) > 43 && (arr ! 4) > 43

leftSide :: Cube -> Bool
{-# INLINE leftSide #-}
leftSide (Cube arr) = div (arr ! 1) 10 `elem` [1, 4, 5, 8] &&
                      div (arr ! 2) 10 `elem` [1, 4, 5, 8] &&
                      div (arr ! 3) 10 `elem` [1, 4, 5, 8] &&
                      div (arr ! 4) 10 `elem` [1, 4, 5, 8]

rightSide :: Cube -> Bool
{-# INLINE rightSide #-}
rightSide (Cube arr) = rightPiece (arr ! 1) && rightPiece (arr ! 2) && rightPiece (arr ! 3) && rightPiece (arr ! 4)

frontSide :: Cube -> Bool
{-# INLINE frontSide #-}
frontSide (Cube arr) = frontPiece (arr ! 1) && frontPiece (arr ! 2) && frontPiece (arr ! 3) && frontPiece (arr ! 4)

backSide :: Cube -> Bool
{-# INLINE backSide #-}
backSide (Cube arr) = backPiece (arr ! 1) && backPiece (arr ! 2) && backPiece (arr ! 3) && backPiece (arr ! 4)

-}



{-
oriented :: Cube -> Bool
oriented (Cube arr) = mod (o8 + o6) 3 == 0 && 
                      mod (o5 + o7) 3 == 0 && 
                      mod (o7 + o8) 3 == 0 && 
                      mod (o1 + o3) 3 == 0 && 
                      mod (o2 + o4) 3 == 0 && 
                      mod (o1 + o2) 3 == 0
    where
        o1 = arr ! 1 `mod` 10
        o2 = arr ! 2 `mod` 10
        o3 = arr ! 3 `mod` 10
        o4 = arr ! 4 `mod` 10
        o5 = arr ! 5 `mod` 10
        o6 = arr ! 6 `mod` 10
        o7 = arr ! 7 `mod` 10
        o8 = arr ! 8 `mod` 10
-}

oriented :: Cube -> Bool
{-# INLINE oriented #-}
oriented (Cube arr) = and (map (\x -> upPiece x || downPiece x) (elems arr)) ||
                      and (map (\x -> leftPiece x || rightPiece x) (elems arr)) ||
                      and (map (\x -> backPiece x || frontPiece x) (elems arr))

orientedPivot :: Cube -> Bool
{-# INLINE orientedPivot #-}
orientedPivot (Cube arr) = mod (arr ! 8) 10 == 0

getCube :: String -> Cube
{-# INLINE getCube #-}
getCube = Cube . listArray (1, 8) . map (\x -> read x :: Word8).words 

checkCube :: Cube -> Bool
checkCube (Cube c) = poss && oris
    where
        poss = [1, 2..8] == (map (\x -> div x 10) . sort . elems $ c)
        oris = 0 == (flip mod 3 $ sum . map (\x -> mod x 10) . elems $ c)
