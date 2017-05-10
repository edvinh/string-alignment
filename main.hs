scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

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

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy fn xs = [x | x <- xs, fn x == maximum(map fn xs)]

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
--optAlignments [] ys = [(replicate (length ys) '-', ys)] -- could've used recursion with attachHeads here instead
--optAlignments xs [] = [(xs, replicate (length xs) '-')]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs []) -- like this
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) con
    where con = concat [ attachHeads x '-' (optAlignments xs (y:ys)),
                       attachHeads '-' y (optAlignments (x:xs) ys),
                       attachHeads x y (optAlignments xs ys) ]

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
    let als = optAlignments s1 s2
    let l = length als
    putStrLn ("There are " ++ show (length als) ++ " optimal alignments: \n")
    mapM_ (putStrLn . split) als
        where 
            split (al1, al2) = al1 ++ "\n" ++ al2 ++ "\n"
    --putStrLn ("There were " ++ show l ++ " optimal alignments!") --parse error, but why?

optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = mcsLen (length xs) (length ys) --still original, change
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> (Int, [AlignmentType])
    mcsEntry 0 0 = (0, [([], [])])
    mcsEntry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
    mcsEntry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
    mcsEntry i j --still original, change
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where --should stay, but more should be added
         x = xs!!(i-1)
         y = ys!!(j-1)