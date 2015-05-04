module TicTacToeAI where

import TicTacToe
import Control.Monad.Random
import qualified PerceptronCoevolutionaryGA as PCG
import qualified Perceptron as P
import CoevolutionaryGA
import Data.List
import GameDataType



a = PCG.perceptronGeneticAlgorithm [27,15,9] 2 fitnessFunc 
t :: (Functor m, MonadRandom m) => [[[([Weight],Weight)]]] -> m [[[([Weight],Weight)]]]
t = geneticAlgorithm3 a
s :: (Functor m, MonadRandom m) => m [[[([Weight],Weight)]]] -> Int -> m [[[([Weight],Weight)]]]
s g n = foldl (>>=) g (replicate n t)

type Move = (Int,Int)

tictactoeAI :: Game Playfield FieldState PlayerVictory Move
tictactoeAI = Game 2 humanPlayer aiPlayer (return initField) playTicTacToe [P1,P2] [P1Won,P2Won] Stalemate NotEnded (g0 a 40) s

playTicTacToe :: Playfield -> [Playfield -> FieldState -> IO Move] -> IO (Playfield,PlayerVictory)
playTicTacToe pf [p1,p2] = playTicTacToe' pf p1 p2 P1

playTicTacToe' pf p1 p2 p = do
    move <- if p == P1 then p1 pf p else p2 pf p
    let pf' = makeMove pf move p
    let vic = evaluateVictory pf' move
    if vic == NotEnded then
      playTicTacToe' pf' p1 p2 p'
    else return (pf',vic)
    where p' = if p == P1 then P2 else P1

aiPlayer :: ([Value] -> [Value]) -> Playfield -> FieldState -> IO (Int,Int)
aiPlayer net pf pID = return $ aiFindMove net pf pID


humanPlayer :: Playfield -> FieldState -> IO (Int,Int)
humanPlayer pf p = do
		print pf
		print $ "your move "++(show p)++"?"
		x <- readLn
		y <- readLn
		let isLegal = (legalMove pf (x,y))
--		let pf' = makeMove pf (x,y) p
		if not isLegal then do
			print "illegal move"
			humanPlayer pf p
		 else
			return (x,y)


fitnessFunc :: [[[([Weight],Weight)]]] -> [([[([Weight],Weight)]],Float)]
fitnessFunc xs@(p1:p2:[]) = (p1,p1Value):(p2,p2Value):[]
                    where initP1 = P.initPerceptrons p1
                          initP2 = P.initPerceptrons p2
                          (victory,turns) = fitnessFunc' initP1 initP2 P1 initField 0
                          p1Value = valueGen P1
                          p2Value = valueGen P2
                          valueGen pID = if victory == (pIDToVictory pID) then 1 else if victory == (pIDToVictory $ otherPlayer pID) then 0 else 0.5

fitnessFuncTest :: Num n => [[[([Weight],Weight)]]] -> ([Float],n)
fitnessFuncTest xs@(p1:p2:[]) = (p1Value:p2Value:[],turns)
                    where initP1 = P.initPerceptrons p1
                          initP2 = P.initPerceptrons p2
                          (victory,turns) = fitnessFunc' initP1 initP2 P1 initField 0
                          p1Value = valueGen P1
                          p2Value = valueGen P2
                          valueGen pID = if victory == (pIDToVictory pID) then 1 else if victory == (pIDToVictory $ otherPlayer pID) then 0 else 0.5
fitnessFuncTest ls = error (show ls)


fitnessFunc' :: Num n => ([Value] -> [Value]) -> ([Value] -> [Value]) -> FieldState -> Playfield -> n -> (PlayerVictory,n)
fitnessFunc' p1 p2 playerTurn pf n = if victory == NotEnded then fitnessFunc' p1 p2 (otherPlayer playerTurn) pf' (n+1) else (victory,n+1)
                            where (pf', victory) = aiTurn activePlayer pf playerTurn
                    	          activePlayer = if playerTurn == P1 then p1 else p2

aiTurn :: ([Value] -> [Value]) -> Playfield -> FieldState -> (Playfield, PlayerVictory)
aiTurn pcg pf playerID = (pf', victory)
                      where move = aiFindMove pcg pf playerID
                            pf' = makeMove pf move playerID
                            enemyWin = pIDToVictory $ otherPlayer playerID
                            victory = evaluateVictory pf' move
                            --victory = if not legal then enemyWin else evaluateVictory pf' move

aiFindMove pcg pf playerID = move
                      where encoded = encodeGame pf playerID
                            --move = decodeMove $ pcg encoded 
                            --legal = legalMove pf move
                            move = findLegalMove (pcg encoded) pf

findLegalMove :: [Value] -> Playfield -> (Int,Int)
findLegalMove movePriority pf = if legal then move else findLegalMove (removeIllegal move movePriority) pf
                        where legal = legalMove pf move
                              move = decodeMove movePriority

removeIllegal :: Integral n => (n,n) -> [Value] -> [Value]
removeIllegal (x,y) vals = replace encoded (\_ -> -100) vals
						where encoded = fromIntegral $ x*3 + y

decodeMove :: Integral n => [Value] -> (n,n)
decodeMove = (\n -> (div n 3, mod n 3)) . snd . head . sortBy (\(a,_) (b,_) -> b `compare` a) . flip zip [0..8]

encodeGame :: Playfield -> FieldState -> [Value]
encodeGame pf playerID = yourStates ++ enemyStates ++ freeStates
                        where   yourStates  = statesGenerator friendlyState
                                enemyStates = statesGenerator enemyState
                                freeStates  = statesGenerator Free
                                statesGenerator fieldID = map (\x -> if x == fieldID then 1 else 0) flatPf
                                friendlyState = playerID
                                enemyState = otherPlayer playerID
                                flatPf = concat pf