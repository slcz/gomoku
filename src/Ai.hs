{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module Ai ( Pos, Dimension, aiInit, aiMove, peerMove, gameFinish, AiState,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
import Data.List ((!!), iterate)
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
    ,   scan      :: [Scan]
    }

data GameResult = GameWin | GameLoss | GameTie deriving (Eq, Show)

wi d = fst d * 3

int2Pos d p = (p `mod` wi d, p `div` wi d)
pos2Int d (x, y) = y * wi d + x

data Scan = Scan
    {
        scanList :: Seq (Seq Int)
    ,   getLine  :: Int -> Int
    }

generateScanList :: Dimension -> [Scan]
generateScanList d = [hScan, vScan, diagRScan, diagLScan] where
    h' = snd d
    w' = fst d
    w  = wi  d
    fList = fromList . map fromList
    hScan = Scan (fList$map (take w'.iterate (+1).(+w').(*w)) [0..h'-1]) (`div` w)
    vScan = Scan (fList$map (take h'.iterate (+w).(+w')) [0..w'-1]) (`mod` w)
    dl = [0 .. w' + w' - 2] :: [Int]
    drophead f = map drop (f [0..w'-1])
    takehead f = map take (f [1..w'-1])
    gen filterF genF findF = Scan (fList $ zipWith id filterF
                (map (take w'.iterate (+findF).genF) dl)) (`mod` findF)
    diagRScan = gen (drophead reverse ++ takehead reverse) (+1) (w+1)
    diagLScan = gen (takehead id ++ drophead id) (+w') (w-1)

-- board-dimension open-move?
aiInit :: Dimension -> Bool -> IO AiState
aiInit d o = return $ AiState
        {
            dimension = d
        ,   open      = o
        ,   me        = mempty
        ,   foe       = mempty
        ,   available = a
        ,   scan      = generateScanList d
        }
    where
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

gameFinish :: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r

-- return deltas of feature set
getFeatures pos scans me foe = getFeatures' pos scans me foe
                                (fromList $ take (2^5) $ repeat 0)

getFeatures :: Int -> [Scan] -> IntSet -> IntSet -> Vector Int
getFeatures' pos scans me foe feat = foldl' (getFeature me foe) feat scans

getFeature :: IntSet -> IntSet -> Vector Int -> Scan -> Vector Int
getFeature me foe feat (Scan sl ln) =  error "get"
