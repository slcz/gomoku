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

-- board-dimension
aiInit :: Dimension -> Int -> IO AiState
aiInit d winningStones =
    return $ AiState
        {
            dimension = d
        ,   winCond   = winningStones
        ,   players   = (mempty, mempty)
        ,   emptySlot = a
        ,   scan      = generateScanList d
        ,   featureMap= (fromList featureMapping, m + 1)
        ,   input     = Data.Vector.replicate (m + m) 0
        ,   theta     = fromList $
            [0.02, 0.015, 0.025, 0.1, 0.01, 0.018, 0.1, 0.1, 0.3, 0.80, 0.005,
             0.15, 0.1,   0.62,  0.61, 1.5,
             -0.024, -0.018, -0.03, -0.12, -0.012, -0.022, -0.12, -0.12, -0.36,
             -0.96, -0.006, -0.18, -0.12, -0.74, -0.71, -1.2]
        }
    where
    a  = setFromList [(x, y) | x <- [0 .. fst d - 1], y <- [0 .. snd d - 1]]
    l' = [0..(2^winningStones - 1)] :: [Int]
    reverseBit i = snd $ foldr reverseBit' (i,0) ([0 .. winningStones - 1] :: [Int])
    reverseBit' b (a,v) = (a `shiftR` 1, (bset `shiftL` b) .|. v)
        where bset = 1 .&. a
    l = map f l'
    f x | popCount x < 2 = 0
    f x | reverseBit x < x = reverseBit x
    f x | otherwise = x
    c = zip (nub l) [0..]
    featureMapping = map fromJust $ map ((flip lookup) c) l
    m = maximumEx featureMapping

evalPos :: Vector Int -> Vector Float -> Float
evalPos v theta = sum $ zipWith (\x y -> fromIntegral x * y) v theta

evalBoard :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> Set Pos -> Vector (Pos, Int)
evalBoard getFeature theta me d positions = map snd $ candidates where
    e    = evalBoard' getFeature theta me d (toList positions)
    emax = fst $ maximumByEx (\x y -> fst x `compare` fst y) e
    candidates = filter (\h -> fst h == emax) e

evalBoard' :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> [Pos] -> Vector (Float, (Pos, Int))
evalBoard' getFeature theta me d []     = mempty
evalBoard' getFeature theta me d (x:xs) =
    (score, (x, pos2Int d x)) `cons` rest where
    (score, delta) = evalBoardOne getFeature theta me d x
    rest = evalBoard' getFeature theta me d xs

evalBoardOne :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float ->
                IntSet -> Dimension -> Pos -> (Float, Bool)
evalBoardOne getFeature theta me d pos = (evalPos feature theta, delta) where
    i        = pos2Int d pos
    me'      = insertSet i me
    feature  = getFeature me me' i
    delta    = any (not . (== 0)) feature

aiMove :: StateT AiState IO Pos
aiMove = do
    AiState d con (m,f) a scans fMap _ the <- get
    let gf m f p = getFeatures fMap p scans m f con
        gfeat m m' p = zipWith (-) (gf m' f p) (gf m f p) ++
                       zipWith (-) (gf f m' p) (gf f m p)
    lst <- return $ evalBoard gfeat the m d a
    idx <- liftIO $ randomRIO (0, length lst - 1)
    (pos, pInt) <- return $ fromJust $ index lst idx
    return pos

peerMove :: Pos -> StateT AiState IO ()
peerMove pos = do
    AiState d con (m,f) a scans fMap fe the <- get
    p     <- return $ pos2Int d pos
    m'    <- return $ insertSet p m
    let gf m f = getFeatures fMap p scans m f con
    delta <- return $
        (zipWith (-) (gf m' f) (gf m f)) ++
        (zipWith (-) (gf f m') (gf f m))
    modify' (\s -> s {  emptySlot = deleteSet pos a
                     ,  players   = (f, m') })
    return ()

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
