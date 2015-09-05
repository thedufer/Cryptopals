module Two.Nine where

import Padding
import StringByteConversion

str = "YELLOW SUBMARINE"

main :: IO ()
main = putStrLn $ bytesToHexString $ pkcs7 20 $ stringToBytes str
