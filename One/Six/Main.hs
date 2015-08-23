import Data.Word
import StringByteConversion (base64StringToBytes, bytesToString)
import Xor (bestMultiByteXor, bestNByteXor)
import Distance (normalizedDistsBetweenBlocks)
import Util (dropNewlines)

main :: IO ()
main = (readFile "One/Six/input.txt") >>= putStrLn . bytesToString . bestMultiByteXor 2 40 . base64StringToBytes . dropNewlines
