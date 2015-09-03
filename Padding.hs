module Padding (pkcs7) where

import Data.Word

requiredPadding :: Int -> [a] -> Int
requiredPadding n as = n - ((length as) `mod` n)

pkcs7 :: Int -> [Word8] -> [Word8]
pkcs7 n bs =
  let reqPad = requiredPadding n bs
  in bs ++ (replicate reqPad $ toEnum reqPad)
