module PerceptronGenetic where

import GA
import System.Random
import Control.Monad.Random
import Perceptron


main = do
	print "generation size:"
	g <- readLn
	print "generations per loop:"
	n <- readLn
	rng <- getStdGen
	genLoop 0 (g0' rng g) n


genLoop gNum acc addGens = do 
	--print acc' 
	print (fitness p' acc') 
	print ("should be: "++ show outp)
	print ("is: "++ show (showFitness acc'))
	print ("generation number:"++ show gNum)
	genLoop (gNum + addGens) (generations p' gaAlgo addGens acc) addGens
	where 	acc' = (bestIndividual p' $ fst acc)

inp  = [[1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1],[1,1,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0]]
outp = [[1,0,0,0,0],[0,1,0,0,0]]
gaAlgo = geneticAlgorithm1

p' = pGA [25,10,5] inp outp
--p' = pGA [2,2,1] [[1,1],[0,0],[0,1],[1,0]] [[1],[0],[0],[0]]
g rng x y = generations p' gaAlgo x (g0' rng y)
g0' rng y = (g0 p' rng y)

--showFitness indv = map (initPerceptrons indv) [[1,1],[0,0],[0,1],[1,0]] 
showFitness indv = map (initPerceptrons indv) inp

f x y = (bestIndividual p' $ (fst $ g (mkStdGen 0) x y))
h x y = fitness p' $ f x y


pGA :: [Int] ->  [[Value]] -> [[Value]] -> GeneticAlgorithm [[([Weight], Weight)]]
pGA sizeLS sourceDataLs targetDataLs = perceptronGeneticAlgorithm sizeLS fitness
	where	fitness x = (1/) . (+1) . sum . map (fitnessHelper x) $ zip sourceDataLs targetDataLs
		fitnessHelper x (sourceData,targetData) = sum . map (abs) . zipWith (-) (initPerceptrons x sourceData) $ targetData


perceptronGeneticAlgorithm :: [Int] -> ([[([Weight], Weight)]] -> Float) -> GeneticAlgorithm [[([Weight], Weight)]]
perceptronGeneticAlgorithm sizeLs f = GeneticAlgorithm f mutate cross generate distance
	where 	mutate (ls,g) = flip runRand g $ do
			(x,y) <- getRandom2DListIndex ls
			v <- getRandomR weightRange
			--return ls
			return (replaceXY x y (foo g (\_ -> v)) ls)
		cross g p1 p2 = flip runRand g $ do
			(x,y) <- getRandom2DListIndex p1
			return $ (replace x . replace y $ (\_ -> p1 !! x !! y)) p2
		generate :: MonadRandom m => m [[([Weight], Weight)]]
		generate = randomPerceptronNet sizeLs
		distance _ _ = 0


foo :: RandomGen g => g -> (a -> a) -> ([a],a) -> ([a],a)
foo g f (ls,b) = flip evalRand g $ do
		x <- getRandomR (0 :: Int, length ls)
		return $ if (x == length ls) then (ls, f b) else (replace x f ls, b)


getRandom2DListIndex :: MonadRandom m => [[a]] -> m (Int, Int)
getRandom2DListIndex ls = do
		x <- getRandomR (0 :: Int,(\x -> x-1) . length $ ls)
		let innerList = head . (drop x) $ ls
		y <- getRandomR (0,(\x -> x-1) . length $ innerList)
		let innerMostList = head . (drop y) $ innerList
		return (x,y)

getRandom3DListElement :: MonadRandom m => [[[a]]] -> m (Int, Int, Int)
getRandom3DListElement ls = do
		x <- getRandomR (0 :: Int,(\x -> x-1) . length $ ls)
		let innerList = head . (drop x) $ ls
		y <- getRandomR (0,(\x -> x-1) . length $ innerList)
		let innerMostList = head . (drop y) $ innerList
		z <- getRandomR (0,(\x -> x-1) . length $ innerMostList)
		return (x,y,z)
			
replace 0 f (x:xs) = f x : xs
replace n f [] = error $ "still had to go deeper:"++(show n)
replace n f (x:xs) = x : replace (n-1) f xs

replaceXYZ x y z v = replace x . replace y . replace z $ (\_ -> v)

replaceXY x y f = replace x . replace y $ f 