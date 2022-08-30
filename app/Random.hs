module Random (rndNext, getRnd, randomChoose) where

import qualified Control.Monad.State.Strict as S
import GHC.Float (int2Double)

rndNext :: Double -> Double
rndNext d
  | d >= 0.3 = (1 - d) / (1 - 0.3)
  | otherwise = d / 0.3

getRnd :: S.State Double Double
getRnd = do
    n <- S.get
    S.put $ rndNext n
    return n

randomChoose :: Double -> [a] -> a
randomChoose r s = s !! floor (r * int2Double (length s))
