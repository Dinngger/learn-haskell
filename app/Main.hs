module Main where

import Debug.Trace (trace)
import System.Environment (getArgs)
import Random (rndNext, getRnd, randomChoose)

import Data.Maybe (catMaybes)
import Data.List (intersect, sort, group)
import qualified Data.Map.Strict as Map

import Graphics.Rendering.Chart.Easy
    ( line, layoutlr_title, plotLeft, plotRight, (.=), layout_title, plot )
import Graphics.Rendering.Chart.Backend.Cairo ()
import Graphics.Rendering.Chart.Gtk ( toWindow )

type State = [Int]
type Memory = Map.Map State Int

showState :: State -> String
showState s = unwords $ concat [replicate n (show i) | (i, n) <- zip [1..(length s - 1)] (drop 1 (reverse s))]

mode :: Bool
mode = True

initState :: State
initState
  | mode = [6, 0, 0, 0, 0, 0, 0]
  | otherwise = [1, 0, 0, 1, 0, 1, 0, 0, 0]

defaultNum :: Int
defaultNum
  | mode = 10000
  | otherwise = 1000

scoreRange :: Int
scoreRange
  | mode = 50
  | otherwise = 10

addOne :: State -> [State]
addOne [] = []
addOne [n] = [[n+1]]
addOne (n:s) = (n+1:s) : map (n:) (addOne s)

tryNextStates :: State -> [State]
tryNextStates [] = []
tryNextStates [n] = [[n]]
tryNextStates (0:s) = map (0:) (tryNextStates s)
tryNextStates (n:s) = map (n-1:) (addOne s) ++ map (n:) (tryNextStates s)

nextStates :: State -> [State]
nextStates s = [x | x <- tryNextStates s, x /= s]

chooseBest :: Double -> ([State], Int) -> [State] -> Memory -> (Double, Maybe State)
chooseBest rnd ([], _) [] m = (rnd, Nothing)
chooseBest rnd (s, i) [] m = (rndNext rnd, Just $ randomChoose rnd s)
chooseBest rnd si (s0:ss) m = combine si (Map.lookup s0 m)
  where combine _ Nothing = (rnd, Just s0)
        combine ([], _) (Just i0) = chooseBest rnd ([s0], i0) ss m
        combine (s1, i1) (Just i0) = chooseBest rnd nsi ss m
          where nsi | i1 > i0 = si | i1 < i0 = ([s0], i0) | otherwise = (s0:s1, i1)

playLoop :: Double -> State -> Memory -> [State] -> (Double, [State])
playLoop rnd s m h = case chooseBest rnd ([], 0) (nextStates s) m of
                      (rnd, Nothing) -> (rnd, s:h)
                      (rnd, Just ns) -> playLoop rnd ns m (s:h)

updateScore :: Int -> Int -> Int
updateScore a b
  | res < -scoreRange = -scoreRange
  | res > scoreRange = scoreRange
  | otherwise = res
  where res = a + b

updateLoop :: Int -> [State] -> Memory -> Memory
updateLoop _ [] m = m
updateLoop w [s] m = Map.insertWith updateScore s w m
updateLoop w (s:ms) m = updateLoop (-w) ms (Map.insertWith updateScore s w m)

update :: Double -> Memory -> (Double, Memory)
update rnd m = case playLoop rnd initState m [] of
                (rndn, ss) -> (rndn, updateLoop (-1) ss m)

trainLoop :: Int -> (Double, [(Int, Int, Int)], Memory) -> (Double, [(Int, Int, Int)], Memory)
trainLoop 0 sm = sm
trainLoop n sm = trainLoop (n+1) $ trainOnce sm
  where trainOnce (rnd, s, m) = (rndn, s2, m2)
          where s2 = case Map.lookup initState m2 of
                      Nothing -> s
                      Just i -> (n, i, Map.size m2):s
                (rndn, m2) = update rnd m

main :: IO ()
main = do
  args <- getArgs
  let num = case args of
              [n] -> read n
              _ -> defaultNum
  let (rnd, datas, mem) = trainLoop (-num) (0.5, [], Map.empty)
      winStates = map fst $ Map.toList $ Map.filter (== -scoreRange) mem
      loseStates = map fst $ Map.toList $ Map.filter (== scoreRange) mem
      loseStatesWin = map (\(s:ss) -> s) . group . sort . concat $ [nextStates s `intersect` loseStates | s <- winStates]
      loseStatesNeed = catMaybes [if all (`elem` winStates) (nextStates s) then Just s else Nothing | s <- loseStatesWin]
  print $ playLoop rnd initState mem []
  putStrLn "All Lose States You Need:"
  putStr . unlines $ map showState loseStatesNeed
  toWindow 1000 500 $ do
    layoutlr_title .= "Training"
    plotLeft (line (if mode then "second win" else "first win") [[(num+i, if mode then n else -n) | (i, n, _) <- datas]])
    plotRight (line "mem size" [[(num+i, n) | (i, _, n) <- datas]])

gen :: Int -> Double -> [(Int, Double)] -> [(Int, Double)]
gen n x s | n == 0 = (n,x):s | otherwise = gen (n-1) (rndNext x) ((n,x):s)

showTent :: IO ()
showTent = do
  toWindow 800 500 $ do
    layout_title .= "Tent"
    plot (line "sample" [gen 500 0.5 []])
