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
    ,   range        :: Range
    ,   win          :: Set Pos
    ,   drawScale    :: Int
    ,   background   :: Color
    ,   stoneSize    :: Rational
    ,   markSize     :: Rational
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
applyOffset board = mapTuple trans $ range board where
    trans = negate . fromIntegral . (* (drawScale board `div` 2))

-- Draw board and stones. First draws grid, then stones of white and black,
-- finally a small square for the group stones that are connected (winner
-- side).
draw :: Board -> Picture
draw board = translate sx sy pic where
    pic = grid <> plays <> wins
    (x, y) = range board
    sc = drawScale board
    (sx, sy) = applyOffset board
    conv = map $ map $ mapTuple $ fromIntegral . (* sc)
    gx = conv [[(x', 0), (x', y )] | x' <- [0 .. x]]
    gy = conv [[(0, y'), (x,  y')] | y' <- [0 .. y]]
    gridfunc = mconcat . map (color black . line)
    grid = gridfunc gx <> gridfunc gy
    trans = fromIntegral . (+ (sc `div` 2)) . (* sc)
    playsfunc location = mconcat
        [   translate (trans mx) (trans my) $ 
            color (stoneColor party)
            (thickCircle 1 ((fromIntegral sc) *
             (fromRational $ stoneSize board)))
        |   (mx, my) <- toList $ stoneSet party] where
        party = location $ opponent board
    plays = playsfunc fst <> playsfunc snd
    mark  = fromRational $ markSize board
    wins  = mconcat [   translate (trans mx) (trans my) $
                        color red
                        (rectangleSolid ((fromIntegral sc) * mark)
                                        ((fromIntegral sc) * mark))
                    |   (mx, my) <- toList $ win board]

-- Capture left mouse button release event.
input :: Event -> Board -> Board
input _ board | not . null . win $ board = board
input (EventKey (MouseButton LeftButton) Up _ (x', y')) board =
    let sc = fromIntegral $ drawScale board
        (sx, sy) = applyOffset board
        stones   = stoneSet $ fst $ opponent board
        allstones = uncurry union $ mapTuple stoneSet $ opponent board
        snap     = floor . (/ sc)
        -- pos@(x, y) is normalized position of the move.
        pos@(x, y) = (snap (x' - sx), snap (y' - sy))
        upd = pos `Data.Set.insert` stones
    in
        if withinBoard (range board) pos &&
           pos `Data.Set.notMember` allstones
            then board {
                opponent = swap ((fst $ opponent board) { stoneSet = upd },
                                 (snd $ opponent board)),
                win  = checkWinCondition upd (range board) pos }
            else board
input _ board = board

step _ = id

initialBoard = Board
    {
        opponent  = (Opponent mempty black, Opponent mempty white)
    ,   range     = (13, 13)
    ,   win       = mempty
    ,   drawScale = 50
    ,   background= makeColor 0.86 0.71 0.52 0.50
    ,   stoneSize = 4 % 5
    ,   markSize  = 1 % 6
    ,   pollInterval = 200
    }

main = do
    let gridSize = range initialBoard
        scaling = (* drawScale initialBoard)
    play (InWindow "GOMOKU" (1, 1) $ mapTuple scaling $ range initialBoard)
         (background initialBoard) (pollInterval initialBoard)
         initialBoard draw input step
