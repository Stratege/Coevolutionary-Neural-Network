module GlobalThermonuclearWarAI where

import GameDataType
import Control.Monad.Random
import qualified PerceptronCoevolutionaryGA as PCG
import CoevolutionaryGA
import GlobalThermonuclearWar
import qualified Perceptron as P
import System.IO.Unsafe

type Playfield = (Player,Player,Int)
type Move = (Troops Int, Int)

gtwAI :: Game Playfield PlayerID Victory Move
gtwAI = Game 2 humanPlayer aiPlayer initField evalField [P1,P2] [P1Won,P2Won] Stalemate NotEnded g0' s

humanPlayer :: Playfield -> PlayerID -> IO (Troops Int, Int)
humanPlayer (p1,p2,t) pID = getPlayerActions p' t
            where p' = if pID == P1 then p1 else p2

aiPlayer :: ([Value] -> [Value]) -> Playfield -> PlayerID -> IO (Troops Int, Int)
aiPlayer pcg (p1,p2,t) pID = return (ai' pcg p' t)
            where p' = if pID == P1 then p1 else p2

initField :: IO Playfield
initField = return (makeStandardPlayer,makeStandardPlayer,0)

evalField :: Playfield -> [Playfield -> PlayerID -> IO Move] -> IO (Playfield,Victory)
evalField pf@(p1,p2,t) [p1C,p2C] = do
    let p1p = 2
    let p2p = 3
    (p1p,p2p) <- playGameWithIO p1C' p2C' (p1,p2) t
    print $ "Player 1 has "++(show p1p)++" territory."
    print $ "Player 2 has "++(show p2p)++" territory."
    return (pf,Stalemate)
    where toUseful pc pID p t = pc (p,p,t) pID
          p1C' = toUseful p1C P1
          p2C' = toUseful p2C P2

g0' :: MonadRandom m => m [[[([Weight],Weight)]]]
g0' = g0 a 40


a = PCG.perceptronGeneticAlgorithm [21,15,9,4] 2 fitnessFunc 
t :: (Functor m, MonadRandom m) => [[[([Weight],Weight)]]] -> m [[[([Weight],Weight)]]]
t = geneticAlgorithm3 a
s :: (Functor m, MonadRandom m) => m [[[([Weight],Weight)]]] -> Int -> m [[[([Weight],Weight)]]]
s g n = foldl (>>=) g (replicate n t)

fitnessFunc :: [[[([P.Weight],P.Weight)]]] -> [([[([P.Weight],P.Weight)]], Float)]
fitnessFunc xs@(x:y:[]) = [(x,fromIntegral p1Fit),(y,fromIntegral p2Fit)] -- [(head xs,0)] 
                    where (p1Fit,p2Fit) = playGame (ai x) (ai y) (makeStandardPlayer,makeStandardPlayer) 0


ai :: [[([P.Weight],Weight)]] -> Player -> Int -> (Troops Int, Int)
ai pcg p timer = ai' pcg' p timer
            where pcg' = P.initPerceptrons pcg
                         
ai' pcg p timer = decodeAction . pcg . encodePlayer p $ timer

--clampToValid (t, sn) (Player c a n t) = ((Player (troopZip min t c), min sn n)

encodePlayer :: Player -> Int -> [Value]
encodePlayer (Player (Troops l w a) _ n t) timer = e l ++ e w ++ e a ++ nEnc ++ tEnc ++ timerEnc
                    where e = encodeIntToVals 5 200
                          nEnc = encodeIntToVals 2 10 n
                          tEnc = encodeIntToVals 2 20000 t
                          timerEnc = encodeIntToVals 2 20 timer


decodeAction :: [Value] -> (Troops Int,Int)
decodeAction (l:w:a:n:[]) = (Troops (e l) (e w) (e a), (d 10 n))
                    where d i = round . (*) i
                          e = d 200

encodeIntToVals bits maxV x = fst g
                where   x' = (fromIntegral x)
                        f y = if y > dv then (1,y / dv) else (y / dv,0)
                        g = foldl (\(g,v) _ -> let (n,m) = f v in (n:g,m)) ([],x') [1..bits]
                        dv = maxV / (2 ^ bits) 