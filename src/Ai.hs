
module Ai ( Pos, Dimension, aiInit, aiMove, peerMove, gameFinish, AiState,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
import Data.IntSet
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
    ,   available :: IntSet
    }

data GameResult = GameWin | GameLoss | GameTie deriving (Eq, Show)

pos2XY :: Int -> Dimension -> Pos
pos2XY p d = (p `div` fst d, p `mod` snd d)

xy2Pos :: Pos -> Dimension -> Int
xy2Pos (x, y) d = x * fst d + y

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
    a = Data.IntSet.fromList [0 .. fst d * snd d - 1]

aiMove :: StateT AiState IO Pos
aiMove = do
    a <- gets available
    m <- gets me
    d <- gets dimension
    let l   = Data.IntSet.toList a
        len = length l
    idx <- liftIO $ (randomRIO (0, len - 1) :: IO Int)
    pos <- return (l !! idx)
    modify' (\s -> s { available = delete pos a, me = insert pos m })
    return $ pos2XY pos d

peerMove :: Pos -> StateT AiState IO ()
peerMove pos = do
    a <- gets available
    f <- gets foe 
    d <- gets dimension
    p <- return $ xy2Pos pos d
    modify' (\s -> s { available = delete p a, foe = insert p f })

gameFinish:: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r

