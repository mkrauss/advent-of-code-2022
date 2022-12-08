data Move = Rock | Paper | Scissors
  deriving (Enum, Read, Show, Eq, Ord)

data Goal = Lose | Draw | Win
  deriving (Enum, Read, Show, Eq, Ord)

translateMove :: String -> Move
translateMove "A" = Rock
translateMove "B" = Paper
translateMove "C" = Scissors

translateGoal :: String -> Goal
translateGoal "X" = Lose
translateGoal "Y" = Draw
translateGoal "Z" = Win

translateResponse :: Move -> Goal -> Move
translateResponse Rock Lose = Scissors
translateResponse Rock Draw = Rock
translateResponse Rock Win = Paper
translateResponse Paper Lose = Rock
translateResponse Paper Draw = Paper
translateResponse Paper Win = Scissors
translateResponse Scissors Lose = Paper
translateResponse Scissors Draw = Scissors
translateResponse Scissors Win = Rock

translateResponseBroken :: String -> Move
translateResponseBroken "X" = Rock
translateResponseBroken "Y" = Paper
translateResponseBroken "Z" = Scissors

translateRoundBroken (theirs:ours:_) =
  (translateMove theirs, translateResponseBroken ours)

translateRound (theirs:ours:_) =
  (translateMove theirs, translateResponse (translateMove theirs) (translateGoal ours))

translateGuide filePath roundTranslator = do
  contents <- readFile filePath
  return $ map (roundTranslator . words) $ lines contents

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

computeScoreFromGuide filePath roundTranslator = do
  guide <- translateGuide filePath roundTranslator
  return $ sum $ map scoreForRound guide

main = do
  brokenScore <- computeScoreFromGuide "input" translateRoundBroken
  score <- computeScoreFromGuide "input" translateRound
  putStr $ "Score for first star: " ++ show brokenScore ++ "\n"
  putStr $ "Score for second star: " ++ show score ++ "\n"
