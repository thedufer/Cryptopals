module Distance (hammingString, hammingBytes, normalizedDistsBetweenBlocks) where

import Data.Word
import Data.Bits (xor, popCount)
import StringByteConversion (stringToBytes)

hammingBytes :: [Word8] -> [Word8] -> Int
hammingBytes xs ys = sum $ map popCount $ zipWith xor xs ys

hammingString :: [Char] -> [Char] -> Int
hammingString xs ys = hammingBytes (stringToBytes xs) (stringToBytes ys)

distBetweenBlocks :: Int -> [Word8] -> Int
distBetweenBlocks size as = hammingBytes (take size as) (take size $ drop size as)

allDistsBetweenBlocks :: Int -> [Word8] -> [Int]
allDistsBetweenBlocks size as
  | (length as >= size * 2) = (distBetweenBlocks size (take (size * 2) as)):(allDistsBetweenBlocks size $ drop (size * 2) as)
  | otherwise               = []

intToFractional :: Fractional a => Int -> a
intToFractional = fromRational . toRational

average :: (Num a, Fractional a) => [a] -> a
average as = (sum as) / (intToFractional $ length as)

normalizedDistsBetweenBlocks :: Fractional a => Int -> Int -> [Word8] -> [a]
normalizedDistsBetweenBlocks min max as = map (\x -> (average $ map intToFractional $ allDistsBetweenBlocks x as) / intToFractional x) [min..max]
