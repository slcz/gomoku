--------------------------------------------------------------------------------
--
-- | Gomoku is a simple board game using haskell gloss library.
-- @
-- Release Notes:
-- For 0.1.0.0
--     Initial checkin, human to human.
--
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Prelude(read)
import ClassyPrelude
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import qualified Data.Set
import Data.Tuple
import Data.Ratio
import Data.Maybe
import Data.List ((!!))
import Data.Set (fromList, delete)
import qualified Data.Text(unpack)
import Control.Concurrent
import System.Random
import System.Exit
import Ai
import Control.Monad.Trans.State.Lazy
import System.Console.GetOpt

data PlayMode = Human | AI deriving (Eq, Show)

data Message = Move Pos | Bug | Result GameResult | Start | Cleanup deriving (Eq, Show)

data Player = Player
    {
        stoneSet    :: Set Pos
    ,   stoneColor  :: Color
    ,   playMode    :: PlayMode
    } deriving (Show)

data Board = Board
    {
        player      :: (Player, Player)
    ,   totalMoves  :: Int
    ,   win         :: Set Pos
    ,   ch          :: (Maybe (TChan Message), Maybe (TChan Message))
    ,   conf        :: Config
    }

(sender, receiver) = (fst, snd)

data Config = Config
    {
        gridSize     :: Int
    ,   dimension    :: Dimension
    ,   background   :: Color
    ,   stoneSize    :: Float
    ,   markSize     :: Float
    ,   pollInterval :: Int
    ,   winCondition :: Int
    ,   margin       :: Int
    ,   textScale    :: Float
    ,   delay        :: Int
    ,   thetaFile    :: FilePath
    ,   mode         :: (PlayMode, PlayMode)
    }

-- check x, y is within the board boundary.
withinBoard (dx, dy) (x, y) = x >= 0 && y >= 0 && x < dx && y < dy

-- maps f to a tuple.
mapTuple f = f *** f

walkDirection :: Set Pos -> Dimension -> Pos -> Pos -> Set Pos
walkDirection set dimension position (deltax, deltay) =
    let oneDirection position'@(x, y) inc =
         if withinBoard dimension position'
           && position' `member` set
            then position' `Data.Set.insert` oneDirection
                    (x + inc * deltax, y + inc * deltay) inc
            else mempty
    in (oneDirection position 1) <> (oneDirection position (-1))

isTie board =
    let d = dimension (conf board) in (fst d) * (snd d) <= totalMoves board

isWin board = not . null . win $ board

getGameEndMsg board | isWin board = (Result GameLoss, Result GameWin)
                    | isTie board = (Result GameTie, Result GameTie)
                    | otherwise   = (Bug, Bug)

--
-- checkWinCondition locations dimension move set-connected-to-the-move
-- A stone is connected to another if it's adjacent
-- either horizontally, vertially or diagnoally.
--
checkWinCondition :: Set Pos -> Dimension -> Pos -> Int -> Set Pos
checkWinCondition set dimension position win =
    let dir :: [Pos]
        dir = [(1, 0), (0, 1), (1, 1), ((-1), 1)]
        walk = walkDirection set dimension position
    in  unions . filter ((== win) . length) . map walk $ dir

-- Declare picture as semigroup in order to use <>
instance Semigroup Picture

-- convinent function to applies offset and locate the board to the
-- center of screen
shiftFun g = negate . fromIntegral . (* (g `div` 2))
shiftx g d = shiftFun g (fst d)
shifty g d = shiftFun g (snd d)

-- Draw board and stones. First draws grid, then stones of white and black,
-- finally a small square for the group stones that are connected (winner
-- side).
draw :: Board -> IO Picture
draw board = return $ translate (shiftx gs d) (shifty gs d) pic where
    Config gs d@(boundaryX, boundaryY) _ ss mark _ _ margin ts _ _ _ = conf board

    pic = grid <> plays <> wins <> context <> gameInfo

    stone = thickCircle 1 ((fromIntegral gs) * ss)
    scaleGrid = map $ map $ mapTuple $ fromIntegral . (* gs)

    gx = scaleGrid [[(x, 0), (x, boundaryY)] | x <- [0 .. boundaryX]]
    gy = scaleGrid [[(0, y), (boundaryX, y)] | y <- [0 .. boundaryY]]
    gridfunc = mconcat . map (color black . line)
    grid = gridfunc gx <> gridfunc gy

    center = fromIntegral . (+ (gs `div` 2)) . (* gs)
    playsfunc location = mconcat
        [   translate (center mx) (center my) $
            color (stoneColor party) stone
        |   (mx, my) <- toList $ stoneSet party] where
        party = location $ player board
    plays = playsfunc fst <> playsfunc snd

    wins  = mconcat [   translate (center mx) (center my) $
                        color red
                        (rectangleSolid ((fromIntegral gs) * mark)
                                        ((fromIntegral gs) * mark))
                    |   (mx, my) <- toList $ win board ]

    contextX = fromIntegral $ gs * (fst d + 1) + margin
    contextY = fromIntegral $ gs * (snd d `div` 2)
    context = translate contextX contextY $
        color (stoneColor $ fst $ player board) stone

    m = show (totalMoves board) ++ "  " ++
        if isTie board || isWin board
            then show $ fst $ getGameEndMsg board
            else ""
    gameInfo = translate (contextX + fromIntegral gs) contextY $
                scale ts ts $ text m

nextState :: Board -> Pos -> IO (Board, Bool)
nextState board pos = do
    let Config gs' dimBoard _ ss mark _ _ _ _ _ _ _ = conf board
        stones   = stoneSet $ fst $ player board
        update = pos `Data.Set.insert` stones
        allstones = uncurry union $ mapTuple stoneSet $ player board
    if withinBoard dimBoard pos &&
       pos `Data.Set.notMember` allstones
        then return (board {
                totalMoves = totalMoves board + 1,
                player = swap ((fst $ player board)
                                    { stoneSet = update },
                                 (snd $ player board)),
                win  = checkWinCondition update dimBoard pos
                        (winCondition $ conf board)},
                True)
        else return (board, False)

-- Capture left mouse button release event.
input :: Event -> Board -> IO Board
input _ board | not . null . win $ board = return board
input _ board | isTie board  = return board
input _ board | (playMode . fst . player $ board) == AI = return board
input (EventKey (MouseButton LeftButton) Up _ (mousex, mousey)) board = do
    let sc = fromIntegral $ gridSize $ conf board
        snap     = floor . (/ sc)
        -- pos@(x, y) is normalized position of the move.
        pos@(x, y) = (snap (mousex - (shiftx g d)), snap (mousey - (shifty g d)))
        (g, d) = (gridSize $ conf board, dimension $ conf board)
    (newBoard, legal) <- nextState board pos
    let r = fromJust $ sender $ ch board
    when legal $ sendmsg r (Move pos) newBoard
    when (isTie newBoard || isWin newBoard) $
        atomically $ writeTChan r (snd $ getGameEndMsg newBoard)
    return newBoard
input _ board = return board

sendmsg rec msg board = do
    atomically $ writeTChan rec $ msg
    when ((playMode $ fst $ player board) == AI) $ do
        atomically $ writeTChan rec $ Start

nextAIMove pos board = fst <$> nextState board pos

step :: Float -> Board -> IO Board
step _ board | (playMode . fst . player $ board) == Human =
    if isTie board || isWin board
        then return $ buildBoard (ch board) (conf board)
        else return board
step _ board = do
    let (chTx, chRx) = mapTuple fromJust $ ch board
    maybeMsg <- atomically $ tryReadTChan chRx
    if not $ isJust maybeMsg
        then return board
        else stepUnblocked board (fromJust maybeMsg)

stepUnblocked :: Board -> Message -> IO Board
stepUnblocked board msg =
    let (chTx, chRx) = mapTuple fromJust $ ch board
    in if isTie board || isWin board
        then return $ buildBoard (ch board) (conf board)
        else
            (threadDelay $ delay $ conf board) >>
            case msg of
            Move pos ->
                do  newBoard <- nextAIMove pos board
                    sendmsg chTx msg newBoard
                    when (isWin newBoard || isTie newBoard) $ atomically $
                            writeTChan chTx (snd $ getGameEndMsg newBoard)
                    return newBoard
            _       -> return board

runAI :: AiState -> (TChan Message, TChan Message) -> Float -> IO AiState
runAI state channels epsilon = do
    let -- Flip sender and receiver at AI agent
        (chTx, chRx) = (receiver channels, sender channels)
    msg <- atomically $ readTChan chRx
    case msg of
        Start -> do
            (p, state') <- runStateT (aiMove epsilon) state
            atomically $ writeTChan chTx $ Move p
            runAI state' channels epsilon
        Move pos -> do  state' <- execStateT (stateChange pos) state
                        runAI state' channels epsilon
        m -> do
            let r = case m of
                        Result r' -> r'
                        _         -> GameTie
            state' <- execStateT (gameFinish r) state
            return state'

startAI :: Board -> (TChan Message, TChan Message) -> Float -> IO ()
startAI board channels epsilon = do
    let c = conf board
    state <- aiInit (dimension c) (winCondition c) (thetaFile c)
    let playmode = mode c
    when (fst playmode == AI) $
        atomically $ (flip writeTChan) Start $ sender channels
    runAI state channels epsilon
    startAI board channels epsilon
    return ()

buildBoard channels config =
    let i = player initialBoard
        playmode = mode config
    in initialBoard {
               ch = channels,
               player = ((fst i) { playMode = fst playmode },
                         (snd i) { playMode = snd playmode }),
               conf = config }

main :: IO ()
main = do
    argv <- getArgs

    let showErr :: [String] -> IO Config
        showErr msgs = ioError (userError (concat msgs ++ usageInfo
                header options))
        header = "Usage: gomoku [OPTION...]"
    config <-
        case getOpt Permute options (map Data.Text.unpack argv) of
            (o,n,[]  ) -> return $ foldl' (flip id) gameConfig o
            (_,_,errs) -> showErr errs

    let playmode = mode config
        ch       = (newTChanIO, newTChanIO)

    epsilon <- return $ if playmode == (AI, AI)
                            then 0.01
                            else 0

    -- create channels for AIs
    channels <- (uncurry $ liftM2 (,)) ch

    board  <- return $ buildBoard (mapTuple Just channels) config
    config <- return $ conf board

    -- fork task for AI agent
    _ <- forkIO (startAI board channels epsilon)

    let scaling = (* gridSize config)
    playIO (InWindow "GOMOKU" (1, 1) $
                mapTuple scaling $ dimension config)
           (background config)
           (pollInterval config)
           board draw input step

-- Initial configurations
initialBoard = Board
    {
        player  = (Player mempty black Human, Player mempty white Human)
    ,   totalMoves= 0
    ,   win       = mempty
    ,   ch        = (Nothing, Nothing)
    ,   conf      = gameConfig
    }

gameConfig = Config
    {
        dimension    = (15, 15)
    ,   gridSize     = 50
    ,   background   = makeColor 0.86 0.71 0.52 0.50
    ,   stoneSize    = fromRational $ 4 % 5
    ,   markSize     = fromRational $ 1 % 6
    ,   pollInterval = 200
    ,   winCondition = 5 -- Win condition: 5 stones connected
    ,   margin       = 20
    ,   textScale    = 0.2
    ,   delay        = 0
    ,   thetaFile    = "theta"
    ,   mode         = (AI, AI)
    }

options :: [OptDescr (Config -> Config)]
options =
    [ Option "d" ["delay"]
        (ReqArg (\d cfg -> cfg { delay = read d }) "USEC")
        "delay microseconds"
    , Option "D" ["dimension"]
        (ReqArg (\d cfg -> cfg { dimension = parseTuple d }) "W,H")
        "board dimension width,height"
    , Option "w" ["wincond"]
        (ReqArg (\d cfg -> cfg { winCondition = read d }) "CONNECTED")
        "number of connected moves to win"
    , Option "p" ["theta"]
        (ReqArg (\d cfg -> cfg { thetaFile = d }) "THETA_FILE")
        "parameter file name"
    , Option "m" ["mode"]
        (ReqArg (\d cfg -> cfg { mode = parseMode d }) "MODE,MODE")
        "playing mode ([human|ai],[human|ai])"
    ] where
    splitComma d = (takeWhile (/= ',') d, tailEx $ dropWhile (/= ',') d)
    parseTuple d = mapTuple read $ splitComma d
    lkmode :: [(String, PlayMode) ]
    lkmode = [("human", Human), ("ai", AI)]
    toMode x = fromJust $ lookup (toLower x) lkmode
    parseMode  d = mapTuple toMode $ splitComma d
