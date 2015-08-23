module Xor (bestNByteXor, bestMultiByteXor, bestSingleByteXor, xorWithBytes, longXor) where

import Data.Word
import Data.Bits (xor)
import Data.List (transpose)
import EnglishDetection (highestScoringBytes)
import Distance (normalizedDistsBetweenBlocks)

xorWithByte :: Word8 -> [Word8] -> [Word8]
xorWithByte b = map (xor b)

longXor :: [Word8] -> [Word8] -> [Word8]
longXor = zipWith xor

bestSingleByteXor :: [Word8] -> [Word8]
bestSingleByteXor bs = highestScoringBytes $ map (flip xorWithByte bs) [0..]

xorWithBytes :: [Word8] -> [Word8] -> [Word8]
xorWithBytes key = longXor (cycle key)

groupsOfN :: Int -> [a] -> [[a]]
groupsOfN _ [] = []
groupsOfN n as = (take n as):groupsOfN n (drop n as)

bestNByteXor :: Int -> [Word8] -> [Word8]
bestNByteXor n as = concat $ transpose $ map bestSingleByteXor $ transpose (groupsOfN n as)

bestMultiByteXor :: Int -> Int -> [Word8] -> [Word8]
bestMultiByteXor min max as =
  flip bestNByteXor as $
    fst $ foldl1
      (\a1 -> \a2 -> if snd a1 > snd a2 then a2 else a1) $
      zip [min..max] $
        normalizedDistsBetweenBlocks min max as
