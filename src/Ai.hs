
module Ai ( Pos, Dimension, aiInit, aiMove, peerMove, gameFinish, AiState,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
-- import Data.IntSet
import Data.List ((!!))
import Control.Monad.Trans.State.Lazy
import System.Random (randomRIO)

type Pos = (Int, Int)
type Dimension = (Int, Int)

data AiState = AiState
    {
        dimension :: Dimension
    ,   open      :: Bool
    ,   me        :: IntSet
    ,   foe       :: IntSet
    ,   available :: Set Pos
    }

data GameResult = GameWin | GameLoss | GameTie deriving (Eq, Show)

int2Pos d p = (p `div` fst d, p `mod` snd d)
pos2Int d (x, y) = x * fst d + y

-- board-dimension open-move?
aiInit :: Dimension -> Bool -> IO AiState
aiInit d o = return $ AiState
    {
        dimension = d
    ,   open      = o
    ,   me        = mempty
    ,   foe       = mempty
    ,   available = a
    } where
    a = setFromList [(x, y) | x <- [0..fst d-1], y <- [0..snd d-1]]

aiMove :: StateT AiState IO Pos
aiMove = do
    a <- gets available
    m <- gets me
    d <- gets dimension
    let l   = toList a
        len = length l
    idx <- liftIO $ (randomRIO (0, len - 1) :: IO Int)
    pos <- return (l !! idx) :: StateT AiState IO Pos
    pInt <- return $ pos2Int d pos
    modify' (\s -> s { available = deleteSet pos a, me = insertSet pInt m })
    return pos

peerMove :: Pos -> StateT AiState IO ()
peerMove pos = do
    a <- gets available
    f <- gets foe 
    d <- gets dimension
    p <- return $ pos2Int d pos
    modify' (\s -> s { available = deleteSet pos a, foe = insertSet p f })

gameFinish:: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r
