--
-- | Gomoku is a simple board game using haskell gloss library.
-- @
-- Release Notes:
-- For 0.1.0.0
-- Initial checkin, human to human.
--
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude()
import ClassyPrelude
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import qualified Data.Set
import Data.Tuple
import Data.Ratio
import Data.Maybe
import Data.List ((!!))
import Data.Set (fromList, delete)
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Debug.Trace
import System.Random

type Dimension = (Int, Int)
type Pos = (Int, Int)

data Player = Human | AI deriving (Eq, Show)

data Message = Move Pos | Win | Lose | Tie | Start deriving (Eq, Show)

data Opponent = Opponent
    {
        stoneSet   :: Set Pos
    ,   stoneColor :: Color
    ,   players    :: Player
    } deriving (Show)

data Board = Board
    {
        opponent    :: (Opponent, Opponent)
    ,   totalMoves  :: Int
    ,   win         :: Set Pos
    ,   ch          :: (Maybe ([TChan Message]), Maybe ([TChan Message]))
    }

data Config = Config
    {
        gridSize     :: Int
    ,   dimension    :: Dimension
    ,   background   :: Color
    ,   stoneSize    :: Float
    ,   markSize     :: Float
    ,   pollInterval :: Int
    ,   winCondition :: Int
    }

-- check x, y is within the board boundary.
withinBoard (dx, dy) (x, y) = x >= 0 && y >= 0 && x < dx && y < dy

-- maps f to a tuple.
mapTuple f = f *** f

walkDirection :: Set Pos -> Dimension -> Pos -> Pos -> Set Pos
walkDirection set rng position@(x, y) (deltax, deltay) =
    let oneDirection position'@(x', y') inc =
         if withinBoard rng position'
           && position' `member` set
            then position' `Data.Set.insert` oneDirection
                    (x' + inc * deltax, y' + inc * deltay) inc
            else mempty
    in (oneDirection position 1) <> (oneDirection position (-1))

checkTieCondition board =
    (fst $ dimension gameConfig) * (snd $ dimension gameConfig) <=
    totalMoves board

--
-- checkWinCondition locations dimension new-stone set-connected-to-the-new-stone.
-- A stone is connected to another if it's adjacent
-- either horizontally, vertially or diagnoally.
--
checkWinCondition :: Set Pos -> Dimension -> Pos -> Set Pos
checkWinCondition set rng position =
    let dir :: [Pos]
        dir = [(1, 0), (0, 1), (1, 1), ((-1), 1)]
        walk = walkDirection set rng position
    in  unions . filter ((==winCondition gameConfig) . length) . map walk $ dir

-- Declare picture as semigroup in order to use <>
instance Semigroup Picture

-- convinent function to applies offset and locate the board to the
-- center of screen
(shiftx, shifty) = mapTuple shiftFun $ dimension gameConfig where
    shiftFun = negate . fromIntegral . (* (gridSize gameConfig `div` 2))

-- Draw board and stones. First draws grid, then stones of white and black,
-- finally a small square for the group stones that are connected (winner
-- side).
draw :: Board -> IO Picture
draw board = return $ translate shiftx shifty pic where
    Config gs (boundaryX, boundaryY) _ ss mark _ _ = gameConfig
    pic = grid <> plays <> wins
    conv = map $ map $ mapTuple $ fromIntegral . (* gs)
    gx = conv [[(x, 0), (x, boundaryY)] | x <- [0 .. boundaryX]]
    gy = conv [[(0, y), (boundaryX, y)] | y <- [0 .. boundaryY]]
    gridfunc = mconcat . map (color black . line)
    grid = gridfunc gx <> gridfunc gy
    center = fromIntegral . (+ (gs `div` 2)) . (* gs)
    playsfunc location = mconcat
        [   translate (center mx) (center my) $
            color (stoneColor party)
            (thickCircle 1 ((fromIntegral gs) * ss))
        |   (mx, my) <- toList $ stoneSet party] where
        party = location $ opponent board
    plays = playsfunc fst <> playsfunc snd
    wins  = mconcat [   translate (center mx) (center my) $
                        color red
                        (rectangleSolid ((fromIntegral gs) * mark)
                                        ((fromIntegral gs) * mark))
                    |   (mx, my) <- toList $ win board]

nextState :: Board -> Pos -> IO (Board, Bool)
nextState board pos = do
    let Config gs' dimBoard _ ss mark _ _ = gameConfig
        stones   = stoneSet $ fst $ opponent board
        update = pos `Data.Set.insert` stones
        allstones = uncurry union $ mapTuple stoneSet $ opponent board
    if withinBoard dimBoard pos &&
       pos `Data.Set.notMember` allstones
        then return (board {
                totalMoves = totalMoves board + 1,
                opponent = swap ((fst $ opponent board)
                                    { stoneSet = update },
                                 (snd $ opponent board)),
                win  = checkWinCondition update dimBoard pos,
                ch   = swap $ ch board }, True)
        else return (board, False)

-- Capture left mouse button release event.
input :: Event -> Board -> IO Board
input _ board | not . null . win $ board = return board
input _ board | checkTieCondition board  = return board
input _ board | (players . fst . opponent $ board) == AI = return board
input (EventKey (MouseButton LeftButton) Up _ (mousex, mousey)) board = do
    let sc = fromIntegral $ gridSize gameConfig
        snap     = floor . (/ sc)
        -- pos@(x, y) is normalized position of the move.
        pos@(x, y) = (snap (mousex - shiftx), snap (mousey - shifty))
    (newBoard, legal) <- nextState board pos
    when legal $ do
        let msgch = snd $ ch board
        when (isJust msgch) $
            atomically $ writeTChan (headEx $ fromJust msgch) $ Move pos
    return newBoard
input _ board = return board

nextAIMove pos board = fst <$> nextState board pos

step :: Float -> Board -> IO Board
step _ board | (players . fst . opponent $ board) == Human = return board
step _ board = do
    let [chTx, chRx] = fromJust . fst . ch $ board
    maybeMsg <- atomically $ tryReadTChan chRx
    if not $ isJust maybeMsg
        then return board
        else stepUnblocked board (fromJust maybeMsg)

stepUnblocked :: Board -> Message -> IO Board
stepUnblocked board msg =
    let
        [chTx, chRx] = fromJust . fst . ch $ board
    in  if checkTieCondition board
        then do atomically $ writeTChan chTx Tie
                let peer = snd $ ch board
                when (isJust peer) $
                    atomically $ writeTChan (headEx $ fromJust peer) Tie
                return board
        else if not . null . win $ board
        then do atomically $ writeTChan chTx Lose
                let peer = snd $ ch board
                when (isJust peer) $
                    atomically $ writeTChan (headEx $ fromJust peer) Win
                return board
        else
            case msg of
            Move pos ->
                do  newBoard <- nextAIMove pos board
                    let peer = fst . ch $ newBoard
                    when (not . null . win $ newBoard) $
                        atomically $ writeTChan chTx Win
                    when (checkTieCondition newBoard) $
                        atomically $ writeTChan chTx Tie
                    when (isJust peer) $
                        atomically $ writeTChan
                        (headEx . fromJust $ peer) msg
                    return newBoard
            _       -> return board

nextMove :: Set (Int, Int) -> IO (Int, Int)
nextMove legalMoves = do
    let l   = toList legalMoves
        len = length legalMoves
    idx <- randomRIO (0, len - 1) :: IO Int
    e   <- return $ l !!idx
    return e
    -- choice

runAI :: [TChan Message] -> Set (Int, Int) -> IO ()
runAI channels legalMoves = do
    let [chRx, chTx] = channels
    msg <- atomically $ readTChan chRx
    case msg of
        Win      -> putStrLn "CLIENT Win"  >> return ()
        Lose     -> putStrLn "CLIENT Lose" >> return ()
        Tie      -> putStrLn "Client Tie"  >> return ()
        Start    -> do
            p  <- nextMove legalMoves
            atomically $ writeTChan chTx $ Move p
            runAI channels $ delete p legalMoves
        Move pos -> do
            lm <- return $ delete pos legalMoves
            p  <- nextMove lm
            atomically $ writeTChan chTx $ Move p
            runAI channels $ delete p legalMoves

newTChanIOpair :: IO [TChan a]
newTChanIOpair = newTChanIO >>= \a -> newTChanIO >>= \b -> return [a, b]

makeChTbl :: [(Player, IO (Maybe [TChan a]))]
makeChTbl = [(Human, return Nothing), (AI, Just <$> newTChanIOpair)]

main :: IO ()
main = do
    let playmode = mapTuple players . opponent $ initialBoard
        ch =  mapTuple (fromJust . flip lookup makeChTbl) playmode

    -- create channels for AIs
    channels <- (uncurry $ liftM2 (,)) ch
    print (isJust $ fst channels, isJust $ snd channels)

    -- fork tasks for AIs
    let legalMoves = Data.Set.fromList [ (x, y) |
                                x <- [0 .. fst (dimension gameConfig) - 1],
                                y <- [0 .. snd (dimension gameConfig) - 1]]
        startIO :: Maybe [TChan Message] -> IO ()
        startIO maybech = case maybech of
                            Nothing-> return ()
                            Just c -> forkIO (runAI c legalMoves) >> return ()
    (startIO $ fst channels) >> (startIO $ snd channels)
    board <- return $ initialBoard {ch = channels }

    -- Sending message to jumpstart the first AI
    atomically $ mapM_ (\x -> writeTChan (headEx x) Start) $ fst channels
    
    let scaling = (* gridSize gameConfig)
    playIO (InWindow "GOMOKU" (1, 1) $
                mapTuple scaling $ dimension gameConfig)
           (background gameConfig)
           (pollInterval gameConfig)
           board draw input step

-- Initial configurations
initialBoard = Board
    {
        opponent  = (Opponent mempty black AI, Opponent mempty white AI)
    ,   totalMoves= 0
    ,   win       = mempty
    ,   ch        = (Nothing, Nothing)
    }

gameConfig = Config
    {
        dimension  = (7, 7)
    ,   gridSize   = 50
    ,   background = makeColor 0.86 0.71 0.52 0.50
    ,   stoneSize  = fromRational $ 4 % 5
    ,   markSize   = fromRational $ 1 % 6
    ,   pollInterval = 200
    ,   winCondition = 5 -- Win condition: 5 stones connected
    }

