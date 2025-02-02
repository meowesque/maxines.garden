module Encoding.UTF8
  ( encode,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)

encode :: Char -> NonEmpty Word8
encode = fmap fromIntegral . go . ord
  where
    go c
      | c <= 0x7f = [c]
      | c <= 0x7ff = [0xc0 + (c `shiftR` 6), 0x80 + c .&. 0x3f]
      | c <= 0xffff =
          [ 0xe0 + (c `shiftR` 12),
            0x80 + ((c `shiftR` 6) .&. 0x3f),
            0x80 + c .&. 0x3f
          ]
      | otherwise =
          [ 0xf0 + (c `shiftR` 18),
            0x80 + ((c `shiftR` 12) .&. 0x3f),
            0x80 + ((c `shiftR` 6) .&. 0x3f),
            0x80 + c .&. 0x3f
          ]
