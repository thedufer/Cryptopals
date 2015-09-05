module Oracle where

import Data.Word

type Oracle = [Word8] -> [Word8]

type MOracle m = [Word8] -> m [Word8]
