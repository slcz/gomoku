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

type Range = (Int, Int)
type Pos   = (Int, Int)

data Board = Board
    {
        stones :: (Set Pos, Set Pos)
    ,   range  :: Range
    }

-- check x, y is within the board boundary.
withinBoard (dx, dy) (x, y) = x >= 0 && y >= 0 && x < dx && y < dy

-- maps f to a tuple.
mapTuple f = f *** f

walkDirection :: Set Pos -> Range -> Pos -> Pos -> Set Pos
walkDirection set rng position@(x, y) (deltax, deltay) =
    let oneDirection position'@(x', y') inc =
         if withinBoard rng position'
           &&
           position' `member` set
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

--
-- [@World@] Global states. Turn is a tuple, the first member is the
-- one that moves first. Win is a set of stones that are connected.
-- drawScale is the scaling factor of the grid.
--
data World = World
    {   board :: Board
    ,   turn  :: (Color, Color)
    ,   win   :: Set Pos
    ,   drawScale :: Int
    }

-- Select the set of stones belonging to a player. Black is the first set.
getSet o = if o == black then id else swap

-- convinent function to applies offset and locate the board to the
-- center of screen
applyOffset world = mapTuple trans (range $ board world) where
    trans = negate . fromIntegral . (* (sc `div` 2))
    sc = drawScale world

-- Draw board and stones. First draws grid, then stones of white and black,
-- finally a small square for the group stones that are connected (winner
-- side).
draw :: World -> Picture
draw world = translate sx sy pic where
    pic = grid <> plays <> wins
    (x, y) = range $ board world
    (sx, sy) = applyOffset world
    sc = drawScale world
    conv = map $ map $ mapTuple $ fromIntegral . (* sc)
    gx = conv [[(x', 0), (x', y )] | x' <- [0 .. x]]
    gy = conv [[(0, y'), (x,  y')] | y' <- [0 .. y]]
    gridfunc = mconcat . map (color black . line)
    grid = gridfunc gx <> gridfunc gy
    trans = fromIntegral . (+ (sc `div` 2)) . (* sc)
    playsfunc opponent = mconcat
        [   translate (trans mx) (trans my) $ 
            color opponent
            (thickCircle 1 ((fromIntegral sc) / 5 * 4))
        |   (mx, my) <- toList . fst . (getSet opponent) . stones . board $ world ]
    plays = playsfunc black <> playsfunc white
    wins  = mconcat [   translate (trans mx) (trans my) $
                        color green 
                        (rectangleSolid ((fromIntegral sc) / 3)
                                        ((fromIntegral sc) / 3))
                    |   (mx, my) <- toList $ win world ]

-- Capture left mouse button release event.
input :: Event -> World -> World
input _ world | not . null . win $ world = world
input (EventKey (MouseButton LeftButton) Up _ (x', y')) world =
    let sc = fromIntegral $ drawScale world
        (sx, sy) = applyOffset world
        brd = board world
        stn = stones brd
        trn = getSet . fst $ turn world
        snap = floor . (/ sc)
        -- pos@(x, y) is normalized position of the move.
        pos@(x, y) = (snap (x' - sx), snap (y' - sy))
        upd' = first (pos `Data.Set.insert`) $ trn stn
        -- update stones on board.
        upd = trn upd'
    in
        if withinBoard (range brd) pos &&
           pos `Data.Set.notMember` (fst stn `union` snd stn)
            then world {
                board = brd { stones = upd },
                turn = swap $ turn world,
                win  = checkWinCondition (fst upd') (range brd) pos }
            else world
input _ world = world

step _ = id

main = do
    let initialBoard = Board (mempty, mempty) (11, 11)
        world = World initialBoard (black, white) mempty 50
        gridSize = range $ board world
        scaling = (* drawScale world)
    play (InWindow "Tic-tac-toe" (1, 1) $
          mapTuple scaling $ range $ board world)
         (makeColor 0.86 0.71 0.52 0.50)
         10 world draw input step
