module OneTwo where

import Data.Word
import StringByteConversion
import Xor (longXor)

input1 = "1c0111001f010100061a024b53535009181c"
input2 = "686974207468652062756c6c277320657965"

main :: IO ()
main = putStrLn $ bytesToHexString $ longXor (hexStringToBytes input1) (hexStringToBytes input2)
