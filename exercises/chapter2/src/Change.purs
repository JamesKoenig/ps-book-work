module Change where

import Prelude
import Data.Int (rem)

leftoverCents :: Int -> Int
leftoverCents = flip rem $ 100
