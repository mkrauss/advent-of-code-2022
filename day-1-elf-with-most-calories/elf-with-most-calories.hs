import Data.Char
import Data.Function
import Data.List
import Data.Ord

--- Learned a lot about sorting in Haskell from:
--- https://ro-che.info/articles/2016-04-02-descending-sort-haskell

getElfCalorieSums = do
  contents <- readFile "input"
  let elfCalorieLists = filter (/= [""])
        $ groupBy ((==) `on` (/= ""))
        $ lines contents
  return (map (sum . (map read)) elfCalorieLists)

main = do
  elfCalorieSums <- getElfCalorieSums
  let topThree = take 3 . sortBy (comparing Down) $ elfCalorieSums
  putStr $ "Elf with most calories: " ++ (show $ maximum elfCalorieSums) ++ "\n"
  putStr $ "Sum of top three elves: " ++ (show $ sum topThree) ++ "\n"
