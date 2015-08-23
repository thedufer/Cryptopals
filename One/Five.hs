module OneFive where

import Xor
import StringByteConversion

input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
key = "ICE"

main :: IO ()
main = putStrLn $ bytesToHexString $ xorWithBytes (stringToBytes key) (stringToBytes input)
