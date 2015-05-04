{-# LANGUAGE Rank2Types #-}
module GA where

import System.Random
import Data.List
import Data.Maybe
import Control.Monad.Random


type GA a g = GeneticAlgorithm a ->  g -> [a] -> ([a],g)

data GeneticAlgorithm a = GeneticAlgorithm { 
		fitness :: a -> Float, 
		mutate :: RandomGen g => (a,g) -> (a,g),
		cross :: RandomGen g => g -> a -> a -> (a,g),
		generate :: MonadRandom m => m a,
		distance :: a -> a -> Int}


generations :: (RandomGen g, Num n, Enum n) => GeneticAlgorithm a -> GA a g -> n -> ([a],g) -> ([a],g)
generations algo ga gens gen = foldl (\(x,g) _ -> ga algo g x) gen [1..gens]
		
generations' algo ga gens rng popSize = generations algo ga gens (g0 algo rng popSize)

g0 algo rng popSize = foldl (\(xs,g) _ -> let (x,g') = (runRand (generate algo) g) in (x:xs,g')) ([],rng) [1..popSize]


--get best individual and mutate the winner of the roulette
geneticAlgorithm1 :: RandomGen g => GeneticAlgorithm a ->  g -> [a] -> ([a],g)
geneticAlgorithm1 algo rng ls = newGen ls rng
                            where   totalFitness xs = sum . map (fitness algo) $ xs
                                    newGen xs g = let (xs',g') = rR xs g; (xs'',g'') = mL xs' g' in ((bestIndividual algo xs) : xs'', g'') 
				    rR xs g = repeatedRoulette algo g xs 1 (totalFitness xs) (popSize-1)
				    popSize = length ls
				    mL xs g = foldl (\(ys,g') y -> let (y',g'') = mutate algo (y,g') in (y':ys,g'')) ([],g) xs


--keep best individual, get winners of roulette, cross them, then mutate the results
geneticAlgorithm2 algo rng ls = newGen ls rng
			    where	totalFitness xs = sum . map (fitness algo) $ xs
					popSize = length ls
					rR xs g = repeatedRoulette algo g xs 1 (totalFitness xs) (popSize-1)
					cL (x:xs) g = foldl (\(y'':ys,g') y -> let (y',g'') = cross algo g' y y'' in (y':y'':ys,g'')) ([x],g) xs
				        mL xs g = foldl (\(ys,g') y -> let (y',g'') = mutate algo (y,g') in (y':ys,g'')) ([],g) xs
					gL g n = foldl (\(xs,g') _ -> let (x',g'') = (runRand (generate algo) g') in (x':xs,g'')) ([],g) [1..n]
					dropCount = floor $ (fromIntegral popSize) / 10
					newGen xs g = let (xs',g') = rR xs g
							  (xs'', g'') = cL xs' g'
							  (xs''', g''') = mL xs'' g''
							  (ds, g'''') = gL g''' dropCount
							  xs'''' = ds ++ drop dropCount xs'''	
							  in ((bestIndividual algo xs) : xs'''', g'''')



				 
bestIndividual algo xs = maximumBy (\a b -> fitness algo a `compare` fitness algo b) xs

repeatedRoulette :: (RandomGen g, Num n, Eq n) => GeneticAlgorithm a -> g -> [a] -> Float -> Float -> n -> ([a],g)
repeatedRoulette algo rng ls minV maxV count = runRand (loop count []) rng
		where 	r' = roulette algo
			loop n acc
			   | n == 0 = return acc
			   | otherwise = do
				v <- getRandomR (minV,maxV)
				loop (n-1) (r' v ls : acc)


roulette :: GeneticAlgorithm a  -> Float -> [a] -> a
roulette algo maxV = (\(_,x) -> fromJust x) . foldl helper (0.0,Nothing)
		where helper (sumV,x) y
			| sumV < maxV = (fitness algo y + sumV,Just y)
			| otherwise   = (sumV,x)


--overselection ::Num n => GeneticAlgorithm a -> [a] -> ([a] -> n -> [a]) -> n -> n -> [a]
overselection algo ls selectionFunc groupFraction endcount =  selectionFunc bestIndividuals 
					where 	bestIndividuals = take takeCount $ sortBy (\x y -> fitness algo x `compare` fitness algo y) ls
						takeCount = floor ((fromIntegral $ length ls) / groupFraction)
						