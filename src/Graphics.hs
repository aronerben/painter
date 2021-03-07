{-# LANGUAGE GADTs #-}

module Graphics
  ( render,
  )
where

import Data.Function
import Data.List (sort, unfoldr)
import qualified Data.Matrix as M
import Data.Maybe
import qualified Data.Vector as V
import Data.Bifunctor

render :: IO ()
render = mapM_ putStrLn $ M.toLists screen

pretty :: M.Matrix Char -> IO ()
pretty = mapM_ putStrLn . M.toLists

width :: Int
width = 60

height :: Int
height = 30

type Screen = M.Matrix Char

screen :: Screen
screen = M.matrix height width (const ' ')

type Coords2D = (Int, Int)

type Coords3D = (Int, Int, Int)

data Shape a where
  Point :: Coords2D -> Shape Coords2D
  Points :: [Coords2D] -> Shape Coords2D
  Line :: Coords2D -> Coords2D -> Shape Coords2D
  Rectangle :: Int -> Int -> Shape Coords2D
  FilledRectangle :: Int -> Int -> Shape Coords2D
  Square :: Int -> Shape Coords2D
  FilledSquare :: Int -> Shape Coords2D
  Translate2D :: Coords2D -> Shape Coords2D -> Shape Coords2D
  Rotate2D :: Int -> Shape Coords2D -> Shape Coords2D
  Scale2D :: Int -> Int -> Shape Coords2D -> Shape Coords2D
  Combine2D :: Shape Coords2D -> Shape Coords2D -> Shape Coords2D
  Cube :: Int -> Shape Coords3D
  Translate3D :: Coords3D -> Shape Coords3D -> Shape Coords3D
  Rotate3D :: Int -> Shape Coords3D -> Shape Coords3D
  Scale3D :: Int -> Int -> Shape Coords3D -> Shape Coords3D
  Combine3D :: Shape Coords3D -> Shape Coords3D -> Shape Coords3D

data InternalShape a where
  InternalRectangle :: Coords2D -> Int -> Int -> InternalShape Coords2D

draw :: Shape a -> Screen -> Screen
draw (Point (x, y)) = fromMaybe <*> M.safeSet '#' (x + 1, y + 1)
draw (Points pts) = \screen -> foldr (draw . Point) screen pts
draw (Line start end) = draw (line start end)
  where
    -- https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm
    line pa@(xa, ya) pb@(xb, yb) = Points $ map maySwitch . unfoldr go $ (x1, y1, 0)
      where
        steep = abs (yb - ya) > abs (xb - xa)
        maySwitch = if steep then (\(x, y) -> (y, x)) else id
        [(x1, y1), (x2, y2)] = sort [maySwitch pa, maySwitch pb]
        deltax = x2 - x1
        deltay = abs (y2 - y1)
        ystep = if y1 < y2 then 1 else -1
        go (xTemp, yTemp, error)
          | xTemp > x2 = Nothing
          | otherwise = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
          where
            tempError = error + deltay
            (newY, newError) =
              if (2 * tempError) >= deltax
                then (yTemp + ystep, tempError - deltax)
                else (yTemp, tempError)
draw (Rectangle w l) = draw $ Points $ (concat [[(0, w'), (l - 1, w')] | w' <- [0 .. w - 1]]) ++ concat [[(l', 0), (l', w - 1)] | l' <- [0 .. l - 1]]
draw (FilledRectangle w l) = draw $ Points $ [(l', w') | w' <- [0 .. w - 1], l' <- [0 .. l - 1]]
draw (Square w) = draw $ Rectangle w w
draw (FilledSquare w) = draw $ FilledRectangle w w
draw (Translate2D diff shape) = draw $ translate2D diff $ draw shape
draw (Rotate2D phi shape) = draw $ rotate2D phi shape

draw' :: [Shape a] -> Screen -> Screen
draw' shapes screen = foldl (flip paint) screen shapes

paint :: Shape a -> Screen -> Screen
paint = undefined

toInternal :: Shape a -> InternalShape a
toInternal (Rectangle w l) = InternalRectangle (0, 0) w l

translate2D :: Coords2D -> InternalShape Coords2D -> InternalShape Coords2D
-- translate2D (dx, dy) (Points pts) = Points $ map (bimap (+ dx) (+ dy)) pts
-- translate2D (dx, dy) (Line (sx, sy) (ex, ey)) = Line (sx + dx, sy + dy) (ex + dx, ey + dy)
translate2D (dx, dy) (InternalRectangle (x, y) w l) = InternalRectangle (x + dx, y + dy) w l
-- rectangle: Points $ (concat [[(dx, w' + dy), (l - 1 + dx, w' + dy)] | w' <- [0 .. w - 1]]) ++ concat [[(l' + dx, dy), (l' + dx, w - 1 + dy)] | l' <- [0 .. l - 1]]
-- translate2D (dx, dy) (FilledRectangle w l) = Points $ [(l' + dx, w' + dy) | w' <- [0 .. w - 1], l' <- [0 .. l - 1]]
-- translate2D diff (Square w) = translate2D diff (Rectangle w w)
-- translate2D diff (FilledSquare w) = translate2D diff (FilledRectangle w w)
-- translate2D diff1 (Translate2D diff2 shape) = translate2D diff1 $ translate2D diff2 shape
-- translate2D diff (Rotate2D phi shape) = translate2D diff $ rotate2D phi shape

rotate2D :: Int -> Shape Coords2D -> Shape Coords2D
rotate2D phi (Square w) = undefined

  -- TODO introduce Path instead of Points for 2D, transform those around
  -- TODO introduce another data structure for 3d, transform those around
  -- differentiate primitive and high level shapes
  -- TODO add static-matrices (maybe)
  -- TODO maybe instead of path use internal structures

-- data Shape a where
--   Point :: Coords2D -> Shape Coords2D
--   Points :: [Coords2D] -> Shape Coords2D
--   Line :: Coords2D -> Coords2D -> Shape Coords2D
--   Rectangle :: Int -> Int -> Shape Coords2D
--   FilledRectangle :: Int -> Int -> Shape Coords2D
--   Square :: Int -> Shape Coords2D
--   FilledSquare :: Int -> Shape Coords2D
--   Translation2D :: Coords2D -> Shape Coords2D -> Shape Coords2D
--   Rotation2D :: Int -> Shape Coords2D -> Shape Coords2D
--   Scale2D :: Int -> Int -> Shape Coords2D -> Shape Coords2D
--   Cube :: Int -> Shape Coords3D
--   Translation3D :: Coords3D -> Shape Coords3D -> Shape Coords3D
--   Rotation3D :: Int -> Shape Coords3D -> Shape Coords3D
--   Scale3D :: Int -> Int -> Shape Coords3D -> Shape Coords3D

box =
  screen
    & draw (Square 20)
    & draw (Line (0, 0) (19, 19))
    & draw (Line (19, 0) (0, 19))
    & pretty
