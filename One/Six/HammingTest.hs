import Data.Word
import Data.Bits (xor, popCount)
import StringByteConversion (stringToBytes)
import Distance (hammingString)

input1 = "this is a test"
input2 = "wokka wokka!!!"

main :: IO ()
main = putStrLn $ show $ hammingString input1 input2
