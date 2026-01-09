module Exercises.Day4.Lib where

import Data.Massiv.Array
import Prelude hiding (sum)

type Diagram = Array B Ix2 Bool

getLocations :: Diagram -> Int
getLocations d = 
    sum 
    . fmap (\x -> if x<4 && x>0 then 1 else 0) 
    . (!*!) d'
    . compute @B
    $ mapStencil (Fill 0) (sumStencil (Sz2 3 3)) d'
    where 
        d' = fromEnum <$> d
