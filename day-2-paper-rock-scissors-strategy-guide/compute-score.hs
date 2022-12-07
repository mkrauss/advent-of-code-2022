data Move = Rock | Paper | Scissors
  deriving (Enum, Read, Show, Eq, Ord)

translateMove :: String -> Move
translateMove "A" = Rock
translateMove "B" = Paper
translateMove "C" = Scissors

translateResponseBroken :: String -> Move
translateResponseBroken "X" = Rock
translateResponseBroken "Y" = Paper
translateResponseBroken "Z" = Scissors

translateRoundBroken (theirs:ours:_) =
  (translateMove theirs, translateResponseBroken ours)

translateGuide filePath = do
  contents <- readFile filePath
  return $ map (translateRoundBroken . words) $ lines contents

scoreForShape :: Move -> Integer
scoreForShape Rock = 1
scoreForShape Paper = 2
scoreForShape Scissors = 3

beats :: Move -> Move -> Bool
beats Scissors Rock = True
beats Paper Scissors = True
beats Rock Paper = True
beats _ _ = False

scoreForOutcome theirs ours
  | beats theirs ours = 6
  | beats ours theirs = 0
  | otherwise = 3

scoreForRound (theirs, ours) = scoreForShape ours + scoreForOutcome theirs ours

computeScoreFromGuide filePath = do
  guide <- translateGuide filePath
  return $ sum $ map scoreForRound guide

main = do
  score <- computeScoreFromGuide "input"
  putStr $ "Score for first star: " ++ show score ++ "\n"
