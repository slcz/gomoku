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
import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import qualified Data.Set
import Data.Tuple
import Data.Ratio

type Range = (Int, Int)
type Pos   = (Int, Int)

data Opponent = Opponent
    {
        stoneSet   :: Set Pos
    ,   stoneColor :: Color
    } deriving (Show)

data Board = Board
    {
        opponent     :: (Opponent, Opponent)
    ,   win          :: Set Pos
    }

data Config = Config
    {
        gridSize     :: Int
    ,   range        :: Range
    ,   background   :: Color
    ,   stoneSize    :: Float
    ,   markSize     :: Float
    ,   pollInterval :: Int
    }

-- check x, y is within the board boundary.
withinBoard (dx, dy) (x, y) = x >= 0 && y >= 0 && x < dx && y < dy

-- maps f to a tuple.
mapTuple f = f *** f

walkDirection :: Set Pos -> Range -> Pos -> Pos -> Set Pos
walkDirection set rng position@(x, y) (deltax, deltay) =
    let oneDirection position'@(x', y') inc =
         if withinBoard rng position'
           && position' `member` set
            then position' `Data.Set.insert` oneDirection
                    (x' + inc * deltax, y' + inc * deltay) inc
            else mempty
    in (oneDirection position 1) <> (oneDirection position (-1))

--
-- checkWinCondition locations range new-stone set-connected-to-the-new-stone.
-- A stone is connected to another if it's adjacent
-- either horizontally, vertially or diagnoally.
--
checkWinCondition :: Set Pos -> Range -> Pos -> Set Pos
checkWinCondition set rng position =
    let dir :: [Pos]
        dir = [(1, 0), (0, 1), (1, 1), ((-1), 1)]
        walk = walkDirection set rng position
    in  unions . filter ((== 5) . length) . map walk $ dir

-- Declare picture as semigroup in order to use <>
instance Semigroup Picture

-- convinent function to applies offset and locate the board to the
-- center of screen
(shiftx, shifty) = mapTuple shiftFun $ range gameConfig where
    shiftFun = negate . fromIntegral . (* (gridSize gameConfig `div` 2))

-- Draw board and stones. First draws grid, then stones of white and black,
-- finally a small square for the group stones that are connected (winner
-- side).
draw :: Board -> Picture
draw board = translate shiftx shifty pic where
    Config gs (boundaryX, boundaryY) _ ss mark _ = gameConfig
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

-- Capture left mouse button release event.
input :: Event -> Board -> Board
input _ board | not . null . win $ board = board
input (EventKey (MouseButton LeftButton) Up _ (mousex, mousey)) board =
    let Config gs' rangeBoard _ ss mark _ = gameConfig
        sc = fromIntegral gs'
        stones   = stoneSet $ fst $ opponent board
        allstones = uncurry union $ mapTuple stoneSet $ opponent board
        snap     = floor . (/ sc)
        -- pos@(x, y) is normalized position of the move.
        pos@(x, y) = (snap (mousex - shiftx), snap (mousey - shifty))
        update = pos `Data.Set.insert` stones
    in
        if withinBoard rangeBoard pos &&
           pos `Data.Set.notMember` allstones
            then board {
                opponent = swap ((fst $ opponent board) { stoneSet = update },
                                 (snd $ opponent board)),
                win  = checkWinCondition update rangeBoard pos }
            else board
input _ board = board

step _ = id

initialBoard = Board
    {
        opponent  = (Opponent mempty black, Opponent mempty white)
    ,   win       = mempty
    }

gameConfig = Config
    {
        range     = (13, 13)
    ,   gridSize  = 50
    ,   background= makeColor 0.86 0.71 0.52 0.50
    ,   stoneSize = fromRational $ 4 % 5
    ,   markSize  = fromRational $ 1 % 6
    ,   pollInterval = 200
    }

main = do
    let scaling = (* gridSize gameConfig)
    play (InWindow "GOMOKU" (1, 1) $ mapTuple scaling $ range gameConfig)
         (background gameConfig)
         (pollInterval gameConfig)
         initialBoard draw input step
