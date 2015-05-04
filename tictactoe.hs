module TicTacToe where





data FieldState = P1 | P2 | Free deriving (Eq, Show)
data PlayerVictory = P1Won | P2Won | Stalemate | NotEnded deriving (Eq, Show)

type Playfield = [[FieldState]]


otherPlayer pID = if pID == P1 then P2 else P1

pIDToVictory P1   = P1Won
pIDToVictory P2   = P2Won
pIDToVictory Free = NotEnded


initField = replicate 3 . replicate 3 $ Free


evaluateVictory :: Playfield -> (Int,Int) -> PlayerVictory
evaluateVictory pf lastMove@(x,y)
			| victory P1 == True = P1Won
			| victory P2 == True = P2Won
			| fieldFree = NotEnded
			| otherwise = Stalemate
			where 	victory p = any (== True) $ diagonal pf p : map (line p) pf ++ map (\x -> vert pf x p) [0..2]
				fieldFree = any (== Free) $ concat pf


line p = all (== p)
vert ls i p = line p . map (!! i) $ ls
diagonal ls p = diagonalHelper 0 (+) p ls || diagonalHelper 2 (-) p ls
diagonalHelper s op p = line p . fst . foldl (\(xs,i) x -> let x' = x !! i in (xs++[x'],i `op` 1)) ([],s)

legalMove :: Playfield -> (Int,Int) -> Bool
legalMove pf (x,y)
	| x < 0 || x > 2 || y < 0 || y > 2 = False
	| otherwise = pf !! x !! y == Free

makeMove :: (Show n, Eq n, Num n) => [[a]] -> (n,n) -> a -> [[a]]
makeMove pf (x,y) p = (replace x . replace y) (\_ -> p) $ pf


replace 0 f (x:xs) = f x : xs
replace n f [] = error $ "still had to go deeper:"++(show n)
replace n f (x:xs) = x : replace (n-1) f xs