module Solutions.Group4 where

import Prelude

import ChapterExamples (Amp(..)
                       ,Volt(..)
                       )

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
-- I'm using e for electromotive force instead of V for voltage b/c as a former
--  Jr electrical engineer I remember awkwardly getting corrected to use E
calculateWattage (Amp i) (Volt e) = Watt (i*e)
