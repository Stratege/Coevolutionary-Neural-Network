module Perceptron where

import Control.Applicative
import System.Random
import Control.Monad.Random
import Control.Monad


type Weight = Float
type Value = Float

weightRange = (-100,100 :: Float)


perceptron :: ([Weight],Weight) -> [Value] -> Value
perceptron (ws,bias) vs 
	| length ws /= length vs = error "need to match lengths"
	| otherwise = 1 / (1 + exp (-1 * net))
		where net = (+bias) . sum $ zipWith (*) ws vs


--bad:	| otherwise = if (sum $ zipWith (\w b -> if b > 0.5 then w else 0.5) ws vs) > 0.5 then 1 else 0

--bool using version
--perceptron ws vs = if helper > 0 then True else False
--		where helper = foldl (\acc (w,v) -> if v then w+acc else acc) 0 $ zip ws vs


initPerceptrons :: [[([Weight],Weight)]] -> [Value] -> [Value]
initPerceptrons ws3d inputs = chainLayers allLayers
		where 	allLayers = map makeLayer ws3d
			chainLayers (x:xs) = foldl (flip chainFunc) (zipWith' (\x y -> x [y]) x inputs) xs
			chainFunc fs ls = [f ls | f <- fs]
			makeLayer ws2d = map perceptron ws2d
			
randomPerceptronNet :: MonadRandom m => [Int] -> m [[([Weight],Weight)]]
randomPerceptronNet ls = liftM (reverse . snd) . foldl (>>=) (anotherHelper2 (head ls) (1, [])) $ zipWith ($) (repeat anotherHelper2) (tail ls)
			where 	f n = do
					xs <- fHelper n
					bias <- getRandomR weightRange
					return (xs,bias)
				fHelper n = sequence $ replicate n (getRandomR weightRange)
				makeLayer prevLen n = sequence $ replicate n (f prevLen)
				anotherHelper prev n acc = do
					new <- makeLayer prev n
					return (length new, new:acc)
				anotherHelper2 n (prev,acc) = anotherHelper prev n acc


--randomPerceptronNet' :: (RandomGen t, Num a, Enum a) => t -> [a] -> [[([Weight],Weight)]]
--randomPerceptronNet' rng ls = fst $ randomPerceptronNet rng ls

zipWith' _ []     []     = []
zipWith' _ []     _      = error "need to match lengths"
zipWith' _ _      []     = error "need to match lengths"
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--p = initPerceptrons (fst $ randomPerceptronNet (mkStdGen 0) [25,10,5])