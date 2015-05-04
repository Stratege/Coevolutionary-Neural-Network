import GA
import System.Random
import Control.Monad.Random


main = do
	print "What color should I approximate?"
	r <- readLn
	g <- readLn
	b <- readLn
	let p' = colorAlgo r g b
	print "generation size:"
	genS <- readLn
	print "generations per loop:"
	n <- readLn
	rng <- getStdGen
	genLoop (r,g,b) p' 0 (g0' p' rng genS) n

genLoop rgb p' gNum acc addGens = do
	print acc' 
	print (fitness p' acc') 
	print ("should be: "++ show rgb)
	print ("is: "++ show acc')
	print ("generation number:"++ show gNum)
	genLoop rgb p' (gNum + addGens) (generations p' geneticAlgorithm2 addGens acc) addGens
	where 	acc' = (bestIndividual p' $ fst acc)


--g p x g' = generations p geneticAlgorithm2 x g'
g0' p rng y = (g0 p rng y)
--f p g = (bestIndividual p . fst $ g)
--h x y = fitness p' $ f x y



colorAlgo r g b = GeneticAlgorithm fitness mutate cross generate distance
	where 	fitness (x,y,z) = 1 / (abs (r-x) + abs (g-y) + abs (b-z) + 1)
		mutate ((x,y,z),g) 
			| c == 1 = ((f x,y,z),g'')
			| c == 2 = ((x,f y,z),g'')
			| c == 3 = ((x,y,f z),g'')
--			| c == 4 = ((f' x,y,z),g'')
--			| c == 5 = ((x,f' y,z),g'')
--			| c == 6 = ((x,y,f' z),g'')
			where   (n,g') = randomR (-10, 10 :: Float) g
				(c,g'') = randomR (1 :: Int,3) g
				f v = clamp 0 255 (v + n)
				f' v = f (-v)
		cross g (x1,y1,z1) (x2,y2,z2) = ((x,y,z),g''')
			where 	(xs,g') = randomR (0,2 :: Int) g
				x = if xs == 1 then x1 else if xs == 2 then x2 else ((x1+x2)/2)
				(ys,g'') = randomR (0,2 :: Int) g'
				y = if ys == 1 then y1 else if ys == 2 then y2 else ((y1+y2)/2)
				(zs,g''') = randomR (0,2 :: Int) g''
				z = if zs == 1 then z1 else if zs == 2 then z2 else ((z1+z2)/2)				
		generate :: MonadRandom m => m (Float,Float,Float)
		generate = do 
			x <- getRandomR (0,255)
			y <- getRandomR (0,255)
			z <- getRandomR (0,255)
			return (x,y,z)
		distance _ _ = 0


clamp min max n
	| n < min = min
	| n > max = max
	| otherwise = n
