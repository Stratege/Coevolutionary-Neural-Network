{-# LANGUAGE Rank2Types #-}
module CoevolutionaryGA where

import System.Random
import Data.List
import Data.Maybe
import Control.Monad.Random
--import qualified Debug.Trace as Debug

type GA a = (Functor m, MonadRandom m) => GeneticAlgorithm a -> [a] -> m [a]

data GeneticAlgorithm a = GeneticAlgorithm { 
		elementsForFitness :: Int,
		fitness :: [a] -> [(a,Float)], 
		mutate :: MonadRandom m => a -> m a,
		cross :: MonadRandom m => a -> a -> m a,
		generate :: MonadRandom m => m a,
		distance :: a -> a -> Int}


generations :: (Functor m, MonadRandom m) => GeneticAlgorithm a -> GA a -> Int -> m [a] -> m [a]
generations algo ga gens gen = foldl (>>=) gen (replicate gens (ga algo))
		
--generations' algo ga gens rng popSize = generations algo ga gens (g0 algo rng popSize)

g0 algo popSize = sequence . replicate popSize $ generate algo

--Debug.trace (show . map snd $ winners')

geneticAlgorithm3 :: GA a
geneticAlgorithm3 algo ls = do
								winners <- tournament2Player algo (1 :: Int) ls
								let winners' = sortBy (\(_,fit1) (_,fit2) -> fit2 `compare` fit1) winners
								new <- (g0 algo remainder)
								mutatedWinners <- sequence $ map (mutate algo . fst) (take fraction winners')
								return $ new ++ mutatedWinners
								where   len = length ls
								        fraction = 9 * (len `div` 10)
								        remainder = len - fraction



geneticAlgorithm2 :: GA a
geneticAlgorithm2 algo ls = do
								winners <- tournament2Player algo (1 :: Int) ls
								--this is in the wrong order, which causes it to be survival of the least fit
								let winners' = sortBy (\(_,fit1) (_,fit2) -> fit1 `compare` fit2) winners
								new <- (g0 algo remainder)
								mutatedWinners <- sequence $ map (mutate algo . fst) (take fraction winners')
								return $ new ++ mutatedWinners
								where   len = length ls
								        fraction = 9 * (len `div` 10)
								        remainder = len - fraction


geneticAlgorithm1 :: GA a
geneticAlgorithm1 algo ls = do
								winners <- tournamentBad algo (10 :: Int) ls
								let winners' = sortBy (\(_,fit1) (_,fit2) -> fit1 `compare` fit2) winners
								new <- (g0 algo remainder)
								return $ new ++ map fst (take fraction winners')
								where   len = length ls
								        fraction = 9 * (len `div` 10)
								        remainder = len - fraction


tournament2Player :: (Num n, Enum n, MonadRandom m, Functor m) => GeneticAlgorithm a -> n -> [a] -> m [(a,Float)]
tournament2Player algo repeatCount ls = if count /= 2 then error "Can only use 2 player tournament with 2 player games" else fmap (concat . fmap (uncurry zip)) . sequence . replicate (len `div` count) $ ff
                    where   randList = sequence (replicate count (pickRandom' ls len))
                            count = elementsForFitness algo
                            len = length ls
                            ff = do
                                ls' <- randList
                                return $ curry fitnessHelper ls' (repeat (0 :: Float))
                            fitnessHelper xs =  unzip $ zipWith (\(a,f1) (_,f2) -> (a,f1+f2)) (p1P2Fitness xs) (p2P1Fitness xs)
                            p1P2Fitness xs = uncurry zip $ fitnessHelperHelper xs
                            p2P1Fitness xs = (\(y:x:[]) -> x:y:[]) . uncurry zip . fitnessHelperHelper . (\((x:y:[]),n) -> (y:x:[],n)) $ xs 
                            fitnessHelperHelper xs = foldl (\(x,fit) _ -> let (x',fit') = unzip . fitness algo $ x in (x',zipWith (+) fit fit')) xs [1..repeatCount]

tournamentBad :: (Num n, Enum n, MonadRandom m, Functor m) => GeneticAlgorithm a -> n -> [a] -> m [(a,Float)]
tournamentBad algo repeatCount ls = fmap (concat . fmap (uncurry zip)) . sequence . replicate (len `div` count) $ ff
                    where   randList = sequence . map (\_ -> pickRandom' ls len) $ [1..count]
                            count = elementsForFitness algo
                            len = length ls
                            ff = do
                                ls' <- randList
                                return $ curry fitnessHelper ls' (repeat (0 :: Float))
                            fitnessHelper xs = foldl (\(x,fit) _ -> let (x',fit') = unzip . fitness algo $ x in (x',zipWith (+) fit fit')) xs [1..repeatCount]


pickRandom :: MonadRandom m => [a] -> m a
pickRandom ls = pickRandom' ls (length ls)

pickRandom' (x:[]) _ = return x
pickRandom' ls len = do
						dropCount <- getRandomR (0 :: Int, len - 1)
						return . head . drop dropCount $ ls

randomSelection :: GeneticAlgorithm a  -> Float -> [a] -> a
randomSelection algo maxV = (\(_,x) -> fromJust x) . foldl helper (0.0,Nothing)
		where helper (sumV,x) y
			| sumV < maxV =  (5 + sumV, Just y) -- (fitness algo y + sumV,Just y)
			| otherwise   = (sumV,x)


--overselection ::Num n => GeneticAlgorithm a -> [a] -> ([a] -> n -> [a]) -> n -> n -> [a]
--overselection algo ls selectionFunc groupFraction endcount =  selectionFunc bestIndividuals 
--					where 	bestIndividuals = take takeCount $ sortBy (\x y -> fitness algo x `compare` fitness algo y) ls
--						takeCount = floor ((fromIntegral $ length ls) / groupFraction)
						