module One.Three where

import Xor
import StringByteConversion

input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

main :: IO ()
main = putStrLn $ bytesToString $ bestSingleByteXor $ hexStringToBytes input
