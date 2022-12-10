import Control.Monad
import Data.List
import Data.Maybe

priority item = fmap (+1)
  $ elemIndex item "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

readRucksacks file = readFile file >>= return . lines

rucksackCompartments r = splitAt (length r `div` 2) r

commonItem (compartment1, compartment2) = nub compartment1 `intersect` compartment2

priorities items = catMaybes $ fmap priority items

elfGroups :: [a] -> [(a, a, a)]
elfGroups (a:b:c:xs) = ((a, b, c) : elfGroups xs)
elfGroups [] = []
elfGroups xs = error "Elves not all in groups of three!"

badge (x, y, z) = nub x `intersect` y `intersect` z

main = do
  rucksacks <- readRucksacks "input"
  let common = fmap rucksackCompartments rucksacks >>= commonItem
  let badges = elfGroups rucksacks >>= badge
  putStr $ "sum of priorities of common items: " ++ show (sum $ priorities common) ++ "\n"
  putStr $ "sum badge priorities of groups: " ++ show (sum $ priorities badges) ++ "\n"
