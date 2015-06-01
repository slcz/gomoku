--------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

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
import Data.Sequence (replicate, update, unstableSortBy, findIndexL,
    ViewL(..), viewl, ViewR(..), viewr, (<|))
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
    ,   available :: Seq (Pos, Int)
    ,   scan      :: [Scan]
    ,   featMap   :: (Vector Int, Int)
    ,   feat      :: Vector Int
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

-- board-dimension open-move?
aiInit :: Dimension -> Int -> Bool -> IO AiState
aiInit d con o =
    return $ AiState
        {
            dimension = d
        ,   conn      = con
        ,   open      = o
        ,   me        = mempty
        ,   foe       = mempty
        ,   available = a
        ,   scan      = generateScanList d
        ,   featMap   = (fromList featureMapping, m + 1)
        ,   feat      = Data.Vector.replicate (m + m) 0
        ,   theta     = fromList $
            [0.02, 0.015, 0.025, 0.1, 0.01, 0.018, 0.1, 0.1, 0.3, 0.80, 0.005,
             0.15, 0.1,   0.62,  0.61, 1,
             -0.024, -0.018, -0.03, -0.12, -0.012, -0.022, -0.12, -0.12, -0.36,
             -0.96, -0.006, -0.18, -0.12, -0.74, -0.71, -1.2]
        }
    where
    a  = fromList [((x, y), 0) | x <- [0..fst d-1], y <- [0..snd d-1]]
    l' = [0..(2^con - 1)] :: [Int]
    reverseBit i = snd $ foldr reverseBit' (i,0) ([0 .. con - 1] :: [Int])
    reverseBit' b (a,v) = (a `shiftR` 1, (bset `shiftL` b) .|. v)
        where bset = 1 .&. a
    l = map f l'
    f x | popCount x < 2 = 0
    f x | reverseBit x < x = reverseBit x
    f x | otherwise = x
    c = zip (nub l) [0..]
    featureMapping = map fromJust $ map ((flip lookup) c) l
    m = maximumEx featureMapping

getDist x y = (fst x - fst y) ^ 2 + (snd x - snd y) ^ 2

updateAvailable old pos = dropWhile ((<0) . snd) $
    unstableSortBy (\x y -> snd x `compare` snd y) updatedDistance where
    updatedDistance = map calcDist old
    calcDist (l, d) = if l == pos
                        then (l, -1)
                        else (l, d + getDist pos l)

evalPos :: Vector Int -> Vector Float -> Float
evalPos v theta = sum $ zipWith (\x y -> fromIntegral x * y) v theta

evalBoard :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> Seq Pos -> Seq (Pos, Int)
evalBoard getFeature theta me d positions = map snd $ candidates where
    e  = evalBoard' getFeature theta me d positions
    e' = unstableSortBy (\x y -> fst x `compare` fst y) e
    maxScore = fst h
    candidates = filter (\h -> fst h == maxScore) e'
    (_ :> h) = viewr e'

evalBoard' :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float -> IntSet
                -> Dimension -> Seq Pos -> Seq (Float, (Pos, Int))
evalBoard' getFeature theta me d positions | viewl positions == EmptyL = mempty
evalBoard' getFeature theta me d positions | otherwise =
    (score, (h, pos2Int d h)) <| restSeq where
    (h :< rest) = viewl positions
    (score, delta) = evalBoardOne getFeature theta me d h
    restSeq = evalBoard' getFeature theta me d rest

evalBoardOne :: (IntSet -> IntSet -> Int -> Vector Int) -> Vector Float ->
                IntSet -> Dimension -> Pos -> (Float, Bool)
evalBoardOne getFeature theta me d pos = (evalPos feature theta, delta) where
    i        = pos2Int d pos
    me'      = insertSet i me
    feature  = getFeature me me' i
    delta    = any (not . (== 0)) feature

aiMove :: StateT AiState IO Pos
aiMove = do
    a     <- gets available
    f     <- gets foe
    m     <- gets me
    d     <- gets dimension
    con   <- gets conn
    fMap  <- gets featMap
    scans <- gets scan
    the   <- gets theta
    let gf m f p = getFeatures fMap p scans m f con
        gfeat m m' p = zipWith (-) (gf m' f p) (gf m f p) ++
                       zipWith (-) (gf f m' p) (gf f m p)
    lst <- return $ evalBoard gfeat the m d $ map fst a
    idx <- liftIO $ randomRIO (0, length lst - 1)
    (pos, pInt) <- return $ fromJust $ index lst idx
    available' <- return $ updateAvailable a pos
    modify' (\s -> s { available = available', me = insertSet pInt m })
    return pos

peerMove :: Pos -> StateT AiState IO ()
peerMove pos = do
    a     <- gets available
    f     <- gets foe
    m     <- gets me
    d     <- gets dimension
    con   <- gets conn
    fMap  <- gets featMap
    fe    <- gets feat
    p     <- return $ pos2Int d pos
    scans <- gets scan
    f'    <- return $ insertSet p f
    available' <- return $ updateAvailable a pos
    let gf m f = getFeatures fMap p scans f m con
    delta <- return $
        (zipWith (-) (gf m f') (gf m f)) ++
        (zipWith (-) (gf f' m) (gf f m))
    modify' (\s -> s { available = available', foe = f',
             feat = zipWith (+) delta fe })
    fe' <- gets feat
    return ()

gameFinish :: GameResult -> StateT AiState IO ()
gameFinish r = liftIO $ putStrLn $ tshow r

-- return deltas of feature set
getFeatures :: (Vector Int, Int) -> Int -> [Scan] -> IntSet -> IntSet ->
                                    Int -> Vector Int
getFeatures featMap pos scans me foe conn =
    drop 1 $ mergeFeat featMap $ getFeatures' pos scans me foe
        (fromList $ take (2^conn) $ repeat 0) conn

mergeFeat :: (Vector Int, Int) -> Vector Int -> Vector Int
mergeFeat (mp,mx) v = foldl' f (Data.Vector.replicate mx 0) (zip mp v) where
    f acc (idx, v) = Data.Vector.modify
                        (\v' -> write v' idx (v + (acc!idx))) acc

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
