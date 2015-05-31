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
import Data.Sequence (sort, replicate, update, unstableSortBy, findIndexL)
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
aiInit d con o = do
    print featureMapping
    return $ AiState
        {
            dimension = d
        ,   conn      = con
        ,   open      = o
        ,   me        = mempty
        ,   foe       = mempty
        ,   available = a
        ,   scan      = generateScanList d
        ,   featMap   = (fromList featureMapping, maximumEx featureMapping + 1)
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

aiMove :: StateT AiState IO Pos
aiMove = do
    a <- gets available
    m <- gets me
    d <- gets dimension
    let len = length a
    idx <- liftIO $ (randomRIO (0, len - 1) :: IO Int)
    ai@(l, _)  <- return $ fromJust $ a `index` idx
    pos <- return l :: StateT AiState IO Pos
    pInt <- return $ pos2Int d pos
    available' <- return $ dropWhile ((>=0).snd) $
        unstableSortBy (\x y -> snd x `compare` snd y) $ update idx (l, -1) a
    modify' (\s -> s { available = available', me = insertSet pInt m })
    return pos

peerMove :: Pos -> StateT AiState IO ()
peerMove pos = do
    a <- gets available
    f <- gets foe 
    m <- gets me
    d <- gets dimension
    con <- gets conn
    fMap <- gets featMap
    p <- return $ pos2Int d pos
    scans <- gets scan
    f'<- return $ insertSet p f
    idx <- return $ fromJust $ Data.Sequence.findIndexL (\x -> fst x == pos) a
    ai@(l, _) <- return $ fromJust $ a `index` idx
    available' <- return $ dropWhile ((>=0).snd) $
        unstableSortBy (\x y -> snd x `compare` snd y) $ update idx (l, -1) a
    modify' (\s -> s { available = available', foe = f' })
    putStrLn $ tshow $ getFeatures fMap p scans f m con
    delta <- return $
        zipWith (-) (getFeatures fMap p scans f' m con)
                    (getFeatures fMap p scans f m con)
    print delta

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
