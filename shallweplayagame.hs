--import TicTacToe
import TicTacToeAI
import GlobalThermonuclearWar as GTW
import GlobalThermonuclearWarAI as GTWAI
import Control.Monad.Random
import qualified Perceptron as P
import Control.Monad
import GameDataType


type Value = Float


main = playQuestion

readGenFile :: String -> String -> IO [[[([Weight],Weight)]]]
readGenFile prePath path = do
    file <- readFile (prePath++"generation"++path)
    return (read file)

aiEval prePath game g n0 = do
    print ("you are in gen "++(show n0)++" - how many gens do you want to add?")
    n <- readLn
    stdGen <- getStdGen
    let res = evalRand (addGens game g n) stdGen
    return ()
    print $ length res
    --print . fitnessFuncTest . take 2 . drop 4 $ res
    writeFile (prePath++"generation"++show (n0+n)) (show res)
    playLoop game res
    print "Add more generations?"
    y <- getLine
    if (y == "y") then do
        aiEval prePath game (return res) (n0+n)
    else
        return()

playLoop game res = do
    print "Do you want to join the game?"
    y <- getLine
    if (y == "y") then do
        humanVSailoop game (drop 4 $ res)
    else
        aiVSailoop game (drop 4 $ res)

aiVSailoop game res = do
    stdGen <- getStdGen
    let (n,g) = randomR (0, length res -1) stdGen
    let (m,g') = randomR (0, length res -1) g
    setStdGen g'
    let pcp1' = (P.initPerceptrons . head . drop n $ res)
    let pcp2' = (P.initPerceptrons . head . drop m $ res)
    startGenericGame game (g_aiPlayer game pcp1') (g_aiPlayer game pcp2') 1
    print "Watch me play again?"
    y <- getLine
    if (y == "y") then do
        aiVSailoop game res
    else
        return ()

humanVSailoop game res = do
    stdGen <- getStdGen
    let (n,g) = randomR (0, length res -1) stdGen
    setStdGen g
    let pcp1' = (P.initPerceptrons . head . drop n $ res)
    startGenericGame game (g_humanPlayer game) (g_aiPlayer game pcp1') 1
    print "Play again?"
    y <- getLine
    if (y == "y") then do
        humanVSailoop game res
    else
        return ()



playQuestion = do
    print "Shall we play a game?"
    y <- getLine
    if (y == "y") then do 
        setupQuestion tictactoeAI "TTT\\"
        playQuestion
    else if (y == "global thermonuclear war") then do
        setupQuestion gtwAI "GTW\\"
        playQuestion
    else return ()

setupQuestion game prePath = do
    print "Load Generation?"
    y <- getLine
    if (y == "y") then do
        print "Enter Generation Number"
        genNum <- getLine
        gen <- readGenFile prePath genNum
        aiEval' game (return gen) (read genNum :: Int)
    else 
        aiEval' game g0'' 0
    where aiEval' game gen gNum = aiEval prePath game gen gNum
          g0'' = aig0 game



gameSelection = do print ()


startGenericGame game p1 p2 startPlayerNum = do
        pf <- (g_initField game)
        playGameGeneric game pf p1 p2 (head . drop (startPlayerNum-1) $ playerStates game)


playGameGeneric game pf p1 p2 p = do
--    pf' <- if p == p1ID then p1 pf p else p2 pf p
    (pf', vic) <- g_playGame game pf [p1,p2]
    when (vic == p1Win) $ print "Player 1 has obtained victory"
    when (vic == p2Win) $ print "Player 2 has won the game"
    when (vic == stalemateState game) $ print "The only winning move is not to play"
    when (vic == gameStillGoingState game) $ playGameGeneric game pf' p1 p2 p'
    where p' = if p == p1ID then p2ID else p1ID
          (p1ID:p2ID:_) = playerStates game
          (p1Win:p2Win:_) = victoryStates game


