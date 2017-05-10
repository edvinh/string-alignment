scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
lstring1 = "aferociousmonadatemyhamster"
lstring2 = "functionalprogrammingrules"

similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys) = maximum [
    similarityScore xs (y:ys) + score x '-',
    similarityScore (x:xs) ys + score '-' y,
    similarityScore xs ys + score x y
  ]
similarityScore xs [] = length xs * scoreSpace
similarityScore [] ys = length ys * scoreSpace

score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
  | x == y    = scoreMatch
  | otherwise = scoreMismatch

attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])] 
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy fn xs = [x | x <- xs, fn x == maximum(map fn xs)]

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) ( -- uncurry: (a -> b -> c) => ((a, b) -> c)
      attachHeads '-' y (optAlignments (x:xs) ys) ++ 
      attachHeads x '-' (optAlignments xs (y:ys)) ++ 
      attachHeads x y (optAlignments xs ys)
    )


optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = map rev (snd $ op (length xs) (length ys))
  where
    rev :: (String, String) -> (String, String)
    rev (s1, s2) = (reverse s1, reverse s2)

    op :: Int -> Int -> (Int, [AlignmentType])
    op i j = table !! i !! j
    table = [[ entry i j | j <- [0..] ] | i <- [0..] ]

    entry :: Int -> Int -> (Int, [AlignmentType])
    entry 0 0 = (0, [([], [])])
    -- using attachHeads instead of attaching to the list tail. We just reverse the strings
    entry i 0 = (scoreSpace + fst (op (i - 1) 0), attachHeads (xs !! (i - 1)) '-' $ snd $ op (i - 1) 0)
    entry 0 j = (scoreSpace + fst (op 0 (j - 1)), attachHeads '-' (ys !! (j - 1)) $ snd $ op 0 (j - 1))
    entry i j = (fst (head res), concatMap snd res) -- concatMap: Maps func over list and concats results (map + concat put together)
      where
        (score1, res1) = op (i - 1) (j - 1)
        (score2, res2) = op (i - 1) j
        (score3, res3) = op i (j - 1)
        x = xs !! (i - 1)
        y = ys !! (j - 1)
        res = maximaBy fst $ [
            (score1 + score x y, attachHeads x y res1),
            (score2 + score x '-', attachHeads x '-' res2),
            (score3 + score '-' y, attachHeads '-' y res3)
          ]



outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
  let als = optAlignments' s1 s2
  putStrLn ("There are " ++ show (length als) ++ " optimal alignments: \n")
  mapM_ (putStrLn . split) als
    where
      split (a, b) = "\n" ++ fn a ++ "\n" ++ fn b
      fn = unwords . map return