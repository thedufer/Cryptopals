module Util (dropNewlines, groupsOfSize) where

dropNewlines :: String -> String
dropNewlines = filter (/= '\n')

groupsOfSize :: Int -> [a] -> [[a]]
groupsOfSize n bs
  | (length bs) >= n = (take n bs):(groupsOfSize n $ drop n bs)
  | otherwise        = []
