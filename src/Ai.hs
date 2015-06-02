{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Ai ( Pos, Dimension, aiInit, aiMove, peerMove, gameFinish, AiState,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
import Data.List ((!!), iterate, nub)
import Data.Vector ((!), modify, replicate)
import Data.Vector.Mutable (write)
import Control.Monad.Trans.State.Lazy
import System.Random (randomRIO)
import Data.Bits
import Data.Maybe (fromJust)
import Debug.Trace

type Pos       = (Int, Int)
type Dimension = (Int, Int)

data AiState = AiState
    {
        dimension :: Dimension
    ,   winCond   :: Int              -- Number of connected stones
    ,   players   :: (IntSet, IntSet) -- first of tuple moves
    ,   emptySlot :: Set Pos
    ,   scan      :: [Scan]
    ,   featureMap:: (Vector Int, Int)
    ,   input     :: Vector Int
    ,   theta     :: Vector Float
    }

data GameResult = GameWin | GameLoss | GameTie deriving (Eq, Show)

wi d = fst d * 3

int2Pos d p = (p `mod` wi d - fst d, p `div` wi d)
pos2Int d (x, y) = y * wi d + x + fst d

-- Generate horizontal/vertical and diagnoal scan lines and
-- functions to lookup for the line.
data Scan = Scan
    {
        scanList :: Vector (Vector Int)
    ,   getLine  :: Int -> Int
    }

generateScanList :: Dimension -> [Scan]
generateScanList d = zipWith build [hScan, vScan, diagRScan, diagLScan]
                                   [hIdx,  vIdx,  diagRIdx,  diagLIdx ] where
    build x y = Scan (fromList $ map fromList x) y
    (h', w', w) = (snd d, fst d, wi d)
    hScan = map (\x -> take w' $ iterate (+1) $ x * w + w') [0..h'-1]
    vScan = map (\x -> take h' $ iterate (+w) $ x + w')     [0..w'-1]
    diagRScan = genDscan diagRClip (+1)  (+(w+1))
    diagLScan = genDscan diagLClip (+w') (+(w-1))

    src    = [0 .. w' + w' - 1] :: [Int]
    drophead f = map drop (f [0..w'-1])
    droptail f = map take (f [1..w'-1])
    diagRClip  = drophead reverse ++ droptail reverse
    diagLClip  = droptail id ++ drophead id
    genDscan clip offset stride = zipWith id clip $ map genDiag src where
        genDiag = take w'. iterate stride . offset

    hIdx       = (`div` w)
    vIdx       = (\x -> x `mod` w - w')
    diagRIdx x = x `mod` (w + 1) - 1
    diagLIdx x = x `mod` (w - 1) - w'

-- board-dimension
aiInit :: Dimension -> Int -> IO AiState
aiInit boardGeom winningStones =
    return $ AiState
        {
            dimension  = boardGeom
        ,   winCond    = winningStones
        ,   players    = (mempty, mempty)
        ,   emptySlot  = emptyBoard
        ,   scan       = generateScanList boardGeom
        ,   featureMap = (fromList featureMapping, m + 1)
        ,   input      = Data.Vector.replicate (m + m) 0
        ,   theta      = fromList $
            [0.02, 0.015, 0.025, 0.1, 0.01, 0.018, 0.1, 0.1, 0.3, 0.80, 0.005,
             0.15, 0.1,   0.62,  0.61, 1.5,
             -0.024, -0.018, -0.03, -0.12, -0.012, -0.022, -0.12, -0.12, -0.36,
             -0.96, -0.006, -0.18, -0.12, -0.74, -0.71, -1.2]
        }
    where
    emptyBoard = setFromList [(x, y) | x <- [0 .. fst boardGeom - 1],
                                       y <- [0 .. snd boardGeom - 1]]
    reverseBit i = snd $ foldr reverseBit' (i,0) ([0 .. winningStones - 1] :: [Int])
    reverseBit' b (a,v) = (a `shiftR` 1, (bset `shiftL` b) .|. v)
        where bset = 1 .&. a
    allPatterns = [0..(2^winningStones - 1)] :: [Int]
    mappings = map filterFeatures allPatterns
    -- A feature is at least two stones in 5 slots,
    -- and merge mirror patterns.
    filterFeatures x | popCount   x < 2 = 0
    filterFeatures x | reverseBit x < x = reverseBit x
    filterFeatures x | otherwise        = x
    compressed = zip (nub mappings) [0..]
    featureMapping = map fromJust $ map ((flip lookup) compressed) mappings
    m = maximumEx featureMapping

value :: Vector Int -> Vector Float -> Float
value input theta = sum $ zipWith (\x y -> fromIntegral x * y) input theta

evaluate :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> Set Pos -> Vector (Pos, Int)
evaluate extract theta black dim positions = map snd $ candidates where
    e    = eval extract theta black dim (toList positions)
    emax = fst $ maximumByEx (\x y -> fst x `compare` fst y) e
    candidates = filter (\h -> fst h == emax) e

eval :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> [Pos] -> Vector (Float, (Pos, Int))
eval _ _ _ _ [] = mempty
eval extract theta black dim (x:xs) =
    (score, (x, pos2Int dim x)) `cons` rest where
    (score, delta) = evalOne extract theta black dim x
    rest = eval extract theta black dim xs

evalOne :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float ->
                IntSet -> Dimension -> Pos -> (Float, Bool)
evalOne extract theta black dim pos = (value feature theta, delta) where
    i        = pos2Int dim pos
    black'   = insertSet i black
    feature  = extract black black' i
    delta    = any (not . (== 0)) feature

aiMove :: StateT AiState IO Pos
aiMove = do
    AiState dimension win (black, white) slot scans featuremap _ parameters
            <- get
    let g black white pos = getFeatures featuremap pos scans black white win
        extFeatures black black' pos =
            zipWith (-) (g black' white pos) (g black white pos) ++
            zipWith (-) (g white black' pos) (g white black pos)
    bestMoves <- return $ evaluate extFeatures parameters black dimension slot
    randCandidate <- liftIO $ randomRIO (0, length bestMoves - 1)
    return $ fst $ fromJust $ index bestMoves randCandidate

peerMove :: Pos -> StateT AiState IO ()
peerMove pos = do
    AiState dimension win (black, white) slot scans featuremap _ parameters
            <- get
    pInt   <- return $ pos2Int dimension pos
    black' <- return $ insertSet pInt black
    let g black white = getFeatures featuremap pInt scans black white win
    delta <- return $
        (zipWith (-) (g black' white ) (g black white)) ++
        (zipWith (-) (g white  black') (g white black))
    modify' (\s -> s {  emptySlot = deleteSet pos slot
                     ,  players   = (white, black') })

gameFinish :: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r

-- return deltas of feature set
getFeatures :: (Vector Int, Int) -> Int -> [Scan] -> IntSet -> IntSet ->
                                    Int -> Vector Int
getFeatures featureMap pos scans me foe win =
    drop 1 $ mergeFeat featureMap $ getFeatures' pos scans me foe
        (fromList $ take (2^win) $ repeat 0) win 

mergeFeat :: (Vector Int, Int) -> Vector Int -> Vector Int
mergeFeat (mp,mx) v = foldl' f (Data.Vector.replicate mx 0) (zip mp v) where
    f acc (idx, v) = Data.Vector.modify
                        (\v' -> write v' idx (v + (acc!idx))) acc

getFeatures' pos scans me foe feat win = foldl' (getFeature pos me foe win)
                                            feat scans

getFeature :: Int -> IntSet -> IntSet -> Int -> Vector Int -> Scan -> Vector Int
getFeature pos me foe win feat (Scan sl ln) =
    getFeature' scanline me foe win feat where
        scanline = sl ! ln pos

getFeature' :: Vector Int -> IntSet -> IntSet -> Int -> Vector Int -> Vector Int
getFeature' sl me foe win feat = fst $ foldl' getfeat (feat, (0, 0)) sl where
    getfeat :: (Vector Int, (Int, Int)) -> Int -> (Vector Int, (Int, Int))
    getfeat (feat, (acc, depth)) pos | pos `member` foe = (feat, (0, 0))
    getfeat (feat, (acc, depth)) pos | pos `member` me =
        check (feat, (acc', depth + 1)) where   acc' = acc + (1 `shiftL` depth)
    getfeat (feat, (acc, depth)) pos | otherwise =
        check (feat, (acc, depth + 1))

    check (feat, (acc, depth)) | depth == win =
        (Data.Vector.modify
                (\v -> write v acc ((feat!acc)+1)) feat, (acc', depth - 1))
        where acc' = acc `shiftR` 1
    check (feat, (acc, depth)) | otherwise = (feat, (acc, depth))
