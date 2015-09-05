module Util (dropNewlines, groupsOfSize, byteStringOfLength) where

import Data.Word
import StringByteConversion (charToByte)

dropNewlines :: String -> String
dropNewlines = filter (/= '\n')

groupsOfSize :: Int -> [a] -> [[a]]
groupsOfSize n bs
  | (length bs) >= n = (take n bs):(groupsOfSize n $ drop n bs)
  | otherwise        = []

byteStringOfLength :: Char -> Int -> [Word8]
byteStringOfLength c n = take n $ repeat $ charToByte c
