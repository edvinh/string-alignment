scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
  | x == y    = scoreMatch
  | otherwise = scoreMismatch

similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys) = maximum [
    similarityScore xs ys + score x y,
    similarityScore xs (y:ys) + score x '-',
    similarityScore (x:xs) ys + score '-' y
  ]
similarityScore xs [] = length xs * scoreSpace
similarityScore [] ys = length ys * scoreSpace

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy fn (x:xs) = [x | x <- xs, fn x == maximum(map fn xs)]

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments [] ys = [(replicate (length ys) '-', ys)] -- could've used recursion with attachHeads here instead
optAlignments xs [] = [(xs, replicate (length xs) '-')]