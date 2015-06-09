-------------------------------------------------------------------------------
-- Temporal difference based learning algorithm.
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TypeFamilies      #-}

module Ai ( Pos, Dimension, aiInit, stateChange, gameFinish, AiState, aiMove,
            GameResult (..)) where

import Prelude()
import ClassyPrelude
import Data.List ((!!), iterate, nub)
import Data.Vector ((!), modify, head, tail)
import Data.Vector.Mutable (write)
import Control.Monad.Trans.State.Lazy
import System.Random (randomRIO)
import Data.Bits
import Data.Maybe (fromJust)
import System.IO (openFile, hClose, IOMode(..))
import Text.Read(read)
import System.Random(randomIO)
import qualified Data.Matrix as M
import Debug.Trace
import System.Directory(copyFile)
import Filesystem.Path(addExtension)

type Pos       = (Int, Int)
type Dimension = (Int, Int)
--
-- W_ih (hid X input),
-- W_ho (1 X hid),
-- b_ih (hid X 1),
-- b_ho (1 X 1)
data Theta = Theta
    {
        wih :: M.Matrix Double
    ,   who :: Vector Double
    ,   bih :: Vector Double
    ,   bho :: Double
    } deriving (Show)

data AiState = AiState
    {
        dimension :: Dimension
    ,   winCond   :: Int              -- Number of connected stones
    ,   players   :: (IntSet, IntSet)
    ,   emptySlot :: Set Pos
    ,   scan      :: [Scan]
    ,   featureMap:: (Vector Int, Int)
    ,   theta     :: Theta
    ,   blackMov :: Bool
    ,   dataset   :: M.Matrix Int
    ,   thetaFname:: FilePath
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

inputSize m = m
hidSize   m = inputSize m `div` 4

readParametersOne :: Int -> FilePath -> IO (Maybe [Double])
readParametersOne m file = do
    let wLen = inputSize m * hidSize m + hidSize m
        bLen = hidSize m + 1
        exp  = wLen + bLen
    handle ((\_ -> return Nothing) :: IOException -> IO (Maybe [Double])) $
            bracket (openFile file ReadMode)
            hClose
            $ \handle -> do
                contents <- hGetContents handle
                w        <- return (words contents)
                doubles  <- return $ map read w :: IO [Double]
                if length doubles /= exp
                    then return Nothing
                    else return $ Just doubles

readParameters :: Int -> FilePath -> IO [Double]
readParameters m file = do
    let altfiles  = [file, file ++ ".bak"] :: [FilePath]
        wLen = inputSize m * hidSize m + hidSize m
        bLen = hidSize m + 1
    wInit      <- replicateM wLen $ ((-)0.5) <$> randomIO :: IO [Double]
    bInit      <- return $ replicate bLen 0.0
    randTheta  <- return $ Just $ wInit ++ bInit
    tryRead    <- mapM (readParametersOne m) altfiles
    readOrRand <- return $ tryRead ++ [randTheta]
    return $ fromJust $ headEx $ filter isJust readOrRand

-- board-dimension
aiInit :: Dimension -> Int -> FilePath -> IO AiState
aiInit boardGeom winningCond thetaFile = do
    parameters <- readParameters m thetaFile
    initialTheta <- return $ unpackTheta m parameters
    return $ AiState
        {
            dimension  = boardGeom
        ,   winCond    = winningCond
        ,   players    = (mempty, mempty)
        ,   emptySlot  = emptyBoard
        ,   scan       = generateScanList boardGeom
        ,   featureMap = (fromList featureMapping, m + 1)
        ,   theta      = initialTheta
        ,   blackMov    = True
        ,   dataset    = M.matrix (inputSize m) 0 $ const 0
        ,   thetaFname = thetaFile
        }
    where
    emptyBoard = setFromList [(x, y) | x <- [0 .. fst boardGeom - 1],
                                       y <- [0 .. snd boardGeom - 1]]
    reverseBit i = snd $ foldr reverseBit' (i,0)
                    ([0 .. winningCond - 1] :: [Int])
    reverseBit' b (a,v) = (a `shiftR` 1, (bset `shiftL` b) .|. v)
        where bset = 1 .&. a
    allPatterns = [0..(2^winningCond - 1)] :: [Int]
    mappings = map filterFeatures allPatterns
    filterFeatures x | popCount   x < 2 = 0
    filterFeatures x | reverseBit x < x = reverseBit x
    filterFeatures x | otherwise        = x
    -- Patterns
    -- ...oo ..o.o
    -- ..oo. ..ooo
    -- .o..o .o.o.
    -- .o.oo .oo.o
    -- .ooo. .oooo
    -- o...o o..oo
    -- o.o.o o.ooo
    -- oo.oo ooooo
    compressed = zip (nub mappings) [0..]
    featureMapping = map fromJust $ map ((flip lookup) compressed) mappings
    m = maximumEx featureMapping

extractFeatures featuremap scans sndMov win fstMov pos =
    zipWith (+) (map (\x -> if x /= 0 then x + 1 else x) a) b
    where
    a = zipWith (-) (g fstMov' sndMov pos) (g fstMov sndMov pos)
    b = zipWith (-) (g sndMov' fstMov pos) (g sndMov fstMov pos)
    fstMov' = insertSet pos fstMov
    sndMov' = insertSet pos sndMov
    g fstMov sndMov pos = getDelta featuremap pos scans fstMov sndMov win

firstSet first = if first then [1, 0] else [0, 1]

aiMove :: Float -> StateT AiState IO Pos
aiMove epsilon = do
    AiState dimension@(dx,dy) win (fstMov, sndMov) slot scans featuremap
            parameters first _ _ <- get
    if (not first) || (not $ null fstMov)
        then do
            rdm <- if epsilon >= 0.000001
                then liftIO $ randomRIO (0, floor (1 / epsilon) :: Int)
                else return 0
            if rdm == 1
                then do
                    size <- liftIO $ randomRIO (0, length slot - 1)
                    return (setToList slot !! size)
                else do
                    let ext = extractFeatures featuremap scans sndMov win
                    bestMoves <- return $ evaluate ext parameters fstMov dimension slot
                    randCandidate <- liftIO $ randomRIO (0, length bestMoves - 1)
                    return $ fst $ fromJust $ index bestMoves randCandidate
        else return (dx `div` 2, dy `div` 2)

stateChange :: Pos -> StateT AiState IO ()
stateChange pos = do
    AiState dimension win (fstMov, sndMov) slot scans featuremap
            parameters first dset _ <- get
    pInt   <- return $ pos2Int dimension pos
    fstMov' <- return $ insertSet pInt fstMov
    allInput <- return $ extractFeatures featuremap scans sndMov win fstMov pInt
    dset'  <- return $ dset M.<|> M.colVector allInput
    modify' (\s -> s {  emptySlot = deleteSet pos slot
                     ,  players   = (sndMov, fstMov')
                     ,  blackMov = not first
                     ,  dataset   = dset' })

gameFinish :: GameResult -> StateT AiState IO ()
gameFinish r = do
    first    <- gets blackMov
    (_, m')  <- gets featureMap
    m        <- return $ m' - 1
    t        <- gets theta
    tF       <- gets thetaFname
    dset     <- gets dataset
    final    <-
        return $ case r of
                    GameTie  -> 0.5
                    _        -> 1.0
    newTheta <- return $ trainNetwork final t dset
    liftIO $ do
        handle ((\_ -> return ()) :: IOException -> IO ()) $
            bracket (openFile tF WriteMode)
            hClose
            $ \handle -> do
                packed <- return $ packTheta m newTheta
                mapM_ (hPutStrLn handle . tshow) packed
        copyFile tF (tF ++ ".bak")
--
-- return input delta after move.
--
getDelta :: (Vector Int, Int) -> Int -> [Scan] -> IntSet -> IntSet ->
            Int -> Vector Int
getDelta (mapping, size) pos scans fstMov sndMov win =
    drop 1 $ compress $ getInputs pos scans fstMov sndMov
        initialValues win where
    initialValues = fromList $ replicate (2 ^ win) 0
    compress v = foldl' f (replicate size 0) (zip mapping v) where
        f values (i, v) = Data.Vector.modify
            (\v' -> write v' i (v + (values ! i))) values

getInputs::Int -> [Scan] -> IntSet -> IntSet -> Vector Int -> Int -> Vector Int
getInputs pos scans fstMov sndMov values win = foldl' getInput values scans
    where
    getInput value (Scan sl ln) = fst $ foldl' getone (value, (0, 0))
        (sl ! ln pos)

    getone (values, (acc, depth)) pos | pos `member` sndMov = (values, (0, 0))
    getone (values, (acc, depth)) pos | pos `member` fstMov =
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
evaluate :: (IntSet -> Int -> Vector Int) -> Theta -> IntSet
                -> Dimension -> Set Pos -> Vector (Pos, Int)
evaluate extract theta fstMov dim positions = map snd $ candidates where
    e    = eval extract theta fstMov dim (toList positions)
    emax = fst $ maximumByEx (\x y -> fst x `compare` fst y) e
    candidates = filter (\h -> fst h == emax) e

unpackTheta :: Int -> [Double] -> Theta
unpackTheta m theta =
    Theta {wih = w1, who = take h w2, bih = take h b1, bho = b2} where
    i = inputSize m
    h = hidSize   m
    w1 = M.fromList h i theta
    w2 = fromList $ drop (i * h) theta
    b1 = drop h w2
    b2 = lastEx b1

packTheta :: Int -> Theta -> Vector Double
packTheta m (Theta w1 w2 b1 b2) =
    packT w1 [1..h] <> w2 <> b1 <> singleton b2 where
    i = inputSize m
    h = hidSize   m
    packT w a = concat $ map ((flip M.getRow) w) $ asVector $ fromList a

eval :: (IntSet -> Int -> Vector Int) -> Theta -> IntSet
                -> Dimension -> [Pos] -> Vector (Double, (Pos, Int))
eval _ _ _ _ [] = mempty
eval extract theta fstMov dim (x:xs) =
    (score, (x, pos2Int dim x)) `cons` rest where
    score = evalOne extract theta fstMov dim x
    rest = eval extract theta fstMov dim xs

-- The first input is special winning conndition (5 connected stones).
-- This causes evaluator to return max (1.0) immediately.
evalOne :: (IntSet -> Int -> Vector Int) -> Theta ->
                IntSet -> Dimension -> Pos -> Double
evalOne extract theta fstMov dim pos = value feature theta where
    i        = pos2Int dim pos
    feature  = extract fstMov i

sigmoid :: Double -> Double
sigmoid t = 1 / (1 + exp(-t))

values :: Vector Int -> Theta -> (Double, Vector Double)
values input (Theta w1 w2 b1 b2) = (out, hid) where
    w1x = M.getCol 1 $ w1 `M.multStd` M.colVector (map fromIntegral input)
    hid = zipWith (\x y -> sigmoid (x + y)) b1 w1x
    w2h = sum $ zipWith (*) w2 hid
    out = sigmoid (w2h + b2)

value :: Vector Int -> Theta -> Double
value input theta = fst $ values input theta

trainNetwork :: Double -> Theta -> M.Matrix Int -> Theta
trainNetwork v' theta dataset = fst $ foldl' trainOne (theta, v') vdata where
    vdata = map ((flip M.getCol) dataset) reverseOrder
    reverseOrder = fromList $ reverse [1..M.ncols dataset] :: Vector Int

trainOne (theta, v') step = (newTheta, prev) where
    (v, h) = values step theta
    target = v' - v
    lambda = 0.8
    prev   = 1 - (v + lambda * target)
    -- newTheta = optim theta h step v target
    newTheta = foldr (\_ theta' -> optim theta' h step v target)
               theta ([1 .. 10] :: [Int])

optim :: Theta -> Vector Double -> Vector Int ->
         Double -> Double -> Theta
optim old hidden input' output target = Theta wh wo bh bo where
    input = map fromIntegral input' :: Vector Double
    (wh, wo, bh, bo) = (wih old, who old, bih old, bho old)
    deltaO = - target * deriv output
    deltaH = zipWith (\a w -> deriv a * w * deltaO) hidden wo
    wo'    = zipWith (\o h -> o - alpha * deltaO * h)
             wo hidden
    bo'    = bo - alpha * deltaO
    temp   = map (alpha *) $ M.colVector deltaH `M.multStd` M.rowVector input
    wh'    = M.matrix (M.nrows wh) (M.ncols wh)
                (\(r, c) -> M.getElem r c wh - M.getElem r c temp)
    bh'    = zipWith (-) bh $ map (alpha *) deltaH
    deriv a = a * (1 - a)
    alpha   = 0.01
