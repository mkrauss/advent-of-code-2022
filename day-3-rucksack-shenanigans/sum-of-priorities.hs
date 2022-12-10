import Data.List
import Data.Maybe

priority item = fmap (+1)
  $ elemIndex item "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

readRucksacks file = do
  contents <- readFile file
  return $ fmap (\l -> splitAt (length l `div` 2) l) $ lines contents

commonItem (compartment1, compartment2) = nub compartment1 `intersect` compartment2

priorities items = catMaybes $ fmap priority items

main = do
  rucksacks <- readRucksacks "input"
  let common = rucksacks >>= commonItem
  putStr $ "result: " ++ show (sum $ priorities common) ++ "\n"
