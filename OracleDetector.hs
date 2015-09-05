module OracleDetector (isECBOrCBC, mIsECBOrCBC) where

import Data.Word

import Oracle
import Util (byteStringOfLength, groupsOfSize)

detectorString :: [Word8]
detectorString = byteStringOfLength 'a' 48

hasEqualNeighbors :: Eq a => [a] -> Bool
hasEqualNeighbors (a1:a2:as)
  | a1 == a2  = True
  | otherwise = hasEqualNeighbors (a2:as)
hasEqualNeighbors _ = False

isECBOrCBC :: Int -> Oracle -> String
isECBOrCBC n f =
  if hasEqualNeighbors (groupsOfSize n $ f detectorString)
     then "ECB"
     else "CBC"

mIsECBOrCBC :: Monad m => Int -> MOracle m -> m String
mIsECBOrCBC n f = do
  out <- f detectorString
  return $
    if hasEqualNeighbors (groupsOfSize n out)
      then "ECB"
      else "CBC"
