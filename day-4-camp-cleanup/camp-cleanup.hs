parsePair delimiter subject =
  let (a, _:b) = break (== delimiter) subject in (a, b)

parsePairOfRanges :: [Char] -> (([Char], [Char]), ([Char], [Char]))
parsePairOfRanges subject =
  let (a, b) = parsePair ',' subject in
    (parsePair '-' a, parsePair '-' b)

parseAssignment :: [Char] -> ((Int, Int), (Int, Int))
parseAssignment subject =
  let ((x1, x2), (y1, y2)) = parsePairOfRanges subject in
    ((read x1, read x2), (read y1, read y2))

parseAssignments input = fmap parseAssignment $ lines input

contains (a1, a2) (b1, b2) = a1 <= b1 && a2 >= b2
redundantAssignment (a, b) = (a `contains` b) || (b `contains` a)
countRedundant = length . filter redundantAssignment

main = do
  input <- readFile "input"
  let assignments = parseAssignments input
  putStr $ "count contains: " ++ show (countRedundant assignments) ++ "\n"
