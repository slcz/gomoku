{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Ai ( Pos, Dimension, aiInit, aiMove, stateChange, gameFinish, AiState,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
import Data.List ((!!), iterate, nub)
import Data.Vector ((!), modify)
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
    ,   firstMove :: Bool
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
        ,   input      = replicate (m + m) 0
        ,   theta      = fromList $
            [0, 0,
             0.02, 0.015, 0.025, 0.1, 0.01, 0.018, 0.1, 0.1, 0.3, 0.80, 0.005,
             0.15, 0.1,   0.62,  0.61, 1.5,
             -0.024, -0.018, -0.03, -0.12, -0.012, -0.022, -0.12, -0.12, -0.36,
             -0.96, -0.006, -0.18, -0.12, -0.74, -0.71, -1.2]
        ,   firstMove  = True
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

extractAllFeatures featuremap scans white win first black black' pos =
    firstSet ++
    zipWith (-) (g black' white pos) (g black white pos) ++
    zipWith (-) (g white black' pos) (g white black pos) where
    firstSet = if first then [1, 0] else [0, 1]
    g black white pos = getDelta featuremap pos scans black white win

aiMove :: StateT AiState IO Pos
aiMove = do
    AiState dimension win (black, white) slot scans featuremap _ parameters
            first <- get
    let ext = extractAllFeatures featuremap scans white win first
    bestMoves <- return $ evaluate ext parameters black dimension slot
    randCandidate <- liftIO $ randomRIO (0, length bestMoves - 1)
    return $ fst $ fromJust $ index bestMoves randCandidate

stateChange :: Pos -> StateT AiState IO ()
stateChange pos = do
    AiState dimension win (black, white) slot scans featuremap input parameters
            first <- get
    pInt   <- return $ pos2Int dimension pos
    black' <- return $ insertSet pInt black
    delta <- return $ extractAllFeatures featuremap scans white win
                      first black black' pInt
    modify' (\s -> s {  emptySlot = deleteSet pos slot
                     ,  players   = (white, black')
                     ,  input     = zipWith (+) input delta })

gameFinish :: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r

--
-- return input delta after move.
--
getDelta :: (Vector Int, Int) -> Int -> [Scan] -> IntSet -> IntSet ->
            Int -> Vector Int
getDelta (mapping, size) pos scans black white win =
    drop 1 $ compress $ getInputs pos scans black white
        initialValues win where
    initialValues = fromList $ replicate (2 ^ win) 0
    compress v = foldl' f (replicate size 0) (zip mapping v) where
        f values (i, v) = Data.Vector.modify
            (\v' -> write v' i (v + (values ! i))) values

getInputs::Int -> [Scan] -> IntSet -> IntSet -> Vector Int -> Int -> Vector Int
getInputs pos scans black white values win = foldl' getInput values scans
    where
    getInput value (Scan sl ln) = fst $ foldl' getone (value, (0, 0)) line where
        line = sl ! ln pos

    getone (values, (acc, depth)) pos | pos `member` white = (values, (0, 0))
    getone (values, (acc, depth)) pos | pos `member` black =
        check (values, (acc', depth + 1)) where acc' = acc + (1 `shiftL` depth)
    getone (values, (acc, depth)) pos | otherwise =
        check (values, (acc, depth + 1))

    check (values, (acc, depth)) | depth == win =
        (Data.Vector.modify
            (\v -> write v acc ((values ! acc) + 1)) values, (acc', depth - 1))
        where acc' = acc `shiftR` 1
    check (values, (acc, depth)) | otherwise = (values, (acc, depth))

--
-- Value Function of the board.
--
evaluate :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> Set Pos -> Vector (Pos, Int)
evaluate extract theta black dim positions = map snd $ candidates where
    e    = eval extract theta black dim (toList positions)
    emax = fst $ maximumByEx (\x y -> fst x `compare` fst y) e
    candidates = filter (\h -> fst h == emax) e

value :: Vector Int -> Vector Float -> Float
value input theta = sum $ zipWith (\x y -> fromIntegral x * y) input theta

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
