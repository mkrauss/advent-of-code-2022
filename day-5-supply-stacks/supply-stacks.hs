import Data.List
import Data.Maybe

moveCrate i j stacks =
  let (as,b:cs) = splitAt (i - 1) stacks
      (xs,y:ys) = splitAt (j - 1) $ as ++ tail b : cs
  in xs ++ [head b:y] ++ ys

parseStacks input = fmap catMaybes $ transpose $ fmap parseLayer input

parseLayer :: String -> [Maybe Char]
parseLayer ('[':c:']':' ':xs) = (Just c:parseLayer xs)
parseLayer ('[':c:']':xs) = (Just c:parseLayer xs)
parseLayer (' ':' ':' ':' ':xs) = (Nothing:parseLayer xs)
parseLayer (' ':' ':' ':xs) = (Nothing:parseLayer xs)
parseLayer _ = []

runStep stacks step =
  let (_:count:_:source:_:destination:_) = words step in
    iterate (moveCrate (read source) (read destination)) stacks !! (read count)

parseInput input =
  let (stackInput, "":stepInput) = (break (== "") . lines) input in
    (parseStacks stackInput, stepInput)

main = do
  input <- readFile "input"
  let (stacks, steps) = parseInput input
  putStr $ "top crates: " ++ (show $ map head $ foldl runStep stacks steps) ++ "\n"
