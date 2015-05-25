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

withinBoard (dx, dy) (x, y) = x >= 0 && y >= 0 && x < dx && y < dy

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

checkWinCondition :: Set Pos -> Range -> Pos -> Set Pos
checkWinCondition set rng position =
    let dir :: [Pos]
        dir = [(1, 0), (0, 1), (1, 1), ((-1), 1)]
        walk = walkDirection set rng position
    in  unions . filter ((== 5) . length) . map walk $ dir

instance Semigroup Picture

data World = World
    {   board :: Board
    ,   turn  :: (Color, Color)
    ,   win   :: Set Pos
    ,   drawScale :: Int
    }

getSet o = if o == black then id else swap

applyOffset world = mapTuple trans (range $ board world) where
    trans = negate . fromIntegral . (* (sc `div` 2))
    sc = drawScale world

draw :: World -> Picture
draw world = translate sx sy pic where
    pic = grid <> plays <> wins
    (x, y) = range $ board world
    (sx, sy) = applyOffset world
    sc = drawScale world
    conv = map $ map $ mapTuple $ fromIntegral . (* sc)
    gx = conv [[(x', 0), (x', y )] | x' <- [0 .. x]]
    gy = conv [[(0, y'), (x,  y')] | y' <- [0 .. y]]
    gridfunc = mconcat . (map (color black . line))
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

input :: Event -> World -> World
input _ world | not . null . win $ world = world
input (EventKey (MouseButton LeftButton) Up _ (x', y')) world =
    let sc = fromIntegral $ drawScale world
        (sx, sy) = applyOffset world
        brd = board world
        stn = stones brd
        trn = getSet . fst $ turn world
        snap = floor . (/ sc)
        pos@(x, y) = (snap (x' - sx), snap (y' - sy))
        upd' = first (pos `Data.Set.insert`) $ trn stn
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
          mapTuple scaling $ range $ board world) azure 10
         world draw input step
