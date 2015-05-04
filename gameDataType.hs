{-# LANGUAGE Rank2Types #-}
module GameDataType where
import Control.Monad.Random


type Weight = Float
type Value = Float

data Game pf pID vic move = Game {
        playerCount :: Int,
        g_humanPlayer :: pf -> pID -> IO move,
        g_aiPlayer :: ([Value] -> [Value]) -> pf -> pID -> IO move,
        g_initField :: IO pf,
        g_playGame :: pf -> [pf -> pID -> IO move] -> IO (pf,vic),
        playerStates :: [pID],
        victoryStates :: [vic],
        stalemateState :: vic,
        gameStillGoingState :: vic,
        aig0 :: MonadRandom m => m [[[([Weight],Weight)]]],
        addGens :: (Functor m, MonadRandom m) => m [[[([Weight],Weight)]]] -> Int -> m [[[([Weight],Weight)]]]
    }