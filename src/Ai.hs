{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module Ai ( Pos, Dimension, aiInit, aiMove, peerMove, gameFinish, AiState,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
import Data.List ((!!), iterate)
import Data.Vector ((!), modify)
import Data.Vector.Mutable (write)
import Control.Monad.Trans.State.Lazy
import System.Random (randomRIO)
import Data.Bits(shiftL, shiftR)
import Debug.Trace

type Pos = (Int, Int)
type Dimension = (Int, Int)

data AiState = AiState
    {
        dimension :: Dimension
    ,   conn      :: Int
    ,   open      :: Bool
    ,   me        :: IntSet
    ,   foe       :: IntSet
    ,   available :: Set Pos
    ,   scan      :: [Scan]
    }

data GameResult = GameWin | GameLoss | GameTie deriving (Eq, Show)

wi d = fst d * 3

int2Pos d p = (p `mod` wi d - fst d, p `div` wi d)
pos2Int d (x, y) = y * wi d + x + fst d

data Scan = Scan
    {
        scanList :: Vector (Vector Int)
    ,   getLine  :: Int -> Int
    }

generateScanList :: Dimension -> [Scan]
generateScanList d = [hScan, vScan, diagRScan, diagLScan] where
    h' = snd d
    w' = fst d
    w  = wi  d
    fList = fromList . map fromList
    hScan = Scan (fList$map (take w'.iterate (+1).(+w').(*w)) [0..h'-1])
            (`div` w)
    vScan = Scan (fList$map (take h'.iterate (+w).(+w')) [0..w'-1])
            (\x -> x `mod` w - w')
    dl = [0 .. w' + w' - 1] :: [Int]
    drophead f = map drop (f [0..w'-1])
    takehead f = map take (f [1..w'-1])
    gen filterF genF off findF = Scan (fList $ zipWith id filterF
                (map (take w'.iterate (+off).genF) dl)) findF
    diagRScan = gen (drophead reverse ++ takehead reverse) (+1) (w+1)
                (\x -> x `mod` (w + 1) - 1)
    diagLScan = gen (takehead id ++ drophead id) (+w') (w-1)
                (\x -> x `mod` (w - 1) - w')

-- board-dimension open-move?
aiInit :: Dimension -> Int -> Bool -> IO AiState
aiInit d con o = return $ AiState
        {
            dimension = d
        ,   conn      = con
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
    m <- gets me
    d <- gets dimension
    con <- gets conn
    p <- return $ pos2Int d pos
    scans <- gets scan
    f'<- return $ insertSet p f
    modify' (\s -> s { available = deleteSet pos a, foe = f' })
    zipWith (-) (getFeatures p scans f' m con) (getFeatures p scans f m con)

gameFinish :: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r

-- return deltas of feature set
getFeatures :: Int -> [Scan] -> IntSet -> IntSet -> Int -> Vector Int
getFeatures pos scans me foe conn = getFeatures' pos scans me foe
                                (fromList $ take (2^conn) $ repeat 0) conn

getFeatures' pos scans me foe feat conn = foldl' (getFeature pos me foe conn)
                                            feat scans

getFeature :: Int -> IntSet -> IntSet -> Int -> Vector Int -> Scan -> Vector Int
getFeature pos me foe conn feat (Scan sl ln) =
    getFeature' scanline me foe conn feat where
        scanline = sl ! ln pos

getFeature' :: Vector Int -> IntSet -> IntSet -> Int -> Vector Int -> Vector Int
getFeature' sl me foe conn feat = fst $ foldl' getfeat (feat, (0, 0)) sl where
    getfeat :: (Vector Int, (Int, Int)) -> Int -> (Vector Int, (Int, Int))
    getfeat (feat, (acc, depth)) pos | pos `member` foe = (feat, (0, 0))
    getfeat (feat, (acc, depth)) pos | pos `member` me =
        check (feat, (acc', depth + 1)) where   acc' = acc + (1 `shiftL` depth)
    getfeat (feat, (acc, depth)) pos | otherwise =
        check (feat, (acc, depth + 1))

    check (feat, (acc, depth)) | depth == conn =
        (Data.Vector.modify
                (\v -> write v acc ((feat!acc)+1)) feat, (acc', depth - 1))
        where acc' = acc `shiftR` 1
    check (feat, (acc, depth)) | otherwise = (feat, (acc, depth))
