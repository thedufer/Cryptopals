module OneEight where

import StringByteConversion
import Data.Word
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Util (groupsOfSize)

hasDupe :: Eq a => [a] -> Bool
hasDupe (x:xs) = (x `elem` xs) || (hasDupe xs)
hasDupe [] = False

hasRepeat :: Int -> [Word8] -> Bool
hasRepeat n bs = hasDupe $ groupsOfSize n bs

main :: IO ()
main = (readFile "One/Eight.txt") >>= putStrLn . intercalate "\n" . groupsOfSize 32 . bytesToHexString . fromJust . (find $ hasRepeat 16) . (map hexStringToBytes) . lines
