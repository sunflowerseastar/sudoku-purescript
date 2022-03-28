module Sudoku where

import Prelude
import Boards (Digit, Grid, Matrix)
import Data.Array (all, any, concat, cons, drop, filter, length, notElem, nub, span, take, uncons, zipWith, (..), (:))
import Data.Foldable (minimum)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

-- ---------
-- solver
-- ---------
boxsize :: Int
boxsize = 3

-- boxsize = 3
digits :: Array Digit
digits = 1 .. (boxsize * boxsize)

blank :: Digit -> Boolean
blank = (==) 0

choices :: Grid -> Matrix (Array Digit)
choices = map (map choice)

choice :: Int -> Array Int
choice d = if blank d then digits else [ d ]

bx :: Array (Array Int)
bx = [ [ 1, 2 ], [ 3, 4 ] ]

bx2 :: Array (Array Int)
bx2 = [ [ 1, 0 ], [ 3, 4 ] ]

cp :: forall a. Array (Array a) -> Array (Array a)
cp matrix = case uncons matrix of
  Just { head: xs, tail: xss } -> do
    x <- xs
    ys <- cp xss
    pure (cons x ys)
  Nothing -> [ [] ]

expand :: Matrix (Array Digit) -> Array Grid
expand = cp <<< map cp

completions :: Grid -> Array Grid
completions = expand <<< choices

nodups :: forall a. Ord a => Array a -> Boolean
nodups arr = length arr == (length $ nub arr)

rows :: forall a. Matrix a -> Matrix a
rows = identity

cols :: forall a. Matrix a -> Matrix a
cols arr = case uncons arr of
  Just { head: xs, tail } -> case length tail > 0 of
    true -> zipWith (:) xs (cols tail)
    false -> do
      x <- xs
      pure [ x ]
  Nothing -> [ [] ]

group :: forall a. Array a -> Array (Array a)
group [] = []

group xs = take boxsize xs : group (drop boxsize xs)

ungroup :: forall a. Array (Array a) -> Array a
ungroup = concat

boxs :: forall a. Matrix a -> Matrix a
boxs = map ungroup <<< ungroup <<< map cols <<< group <<< map group

valid :: Grid -> Boolean
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

single :: forall a. (Array a) -> Boolean
single xs = length xs == 1

remove :: Array Digit -> Array Digit -> Array Digit
remove singletons xs =
  if single xs then
    xs
  else
    filter (\x -> notElem x singletons) xs

singletonsFromArr :: Array (Array Digit) -> Array Digit
singletonsFromArr arr = arr # filter (\x -> length x == 1) # concat

pruneRow :: Array (Array Digit) -> Array (Array Digit)
pruneRow row = map (remove singletonsFromArrArr) row
  where
  singletonsFromArrArr = singletonsFromArr row

pruneBy :: (Array (Matrix Int) -> Array (Matrix Int)) -> Array (Matrix Int) -> Array (Matrix Int)
pruneBy f = f <<< map pruneRow <<< f

prune :: Matrix (Array Digit) -> Matrix (Array Digit)
prune = pruneBy boxs <<< pruneBy cols <<< pruneBy rows

fixedPoint :: forall a. (Eq a) => (a -> a) -> a -> a
fixedPoint f x = if x == y then x else fixedPoint f y
  where
  y = f x

-- counts [[[0],[1,2],[3],[1,3,4],[5,6]],[[0],[1,2],[3],[1,3,4],[5,6]]] => [2,3,2,2,3,2]
counts :: Array (Array (Array Int)) -> Array Int
counts = filter (\x -> x /= 1) <<< map length <<< concat

-- > break (\x -> x > 4) [1, 3, 7, 6, 2, 3, 5] => { init: [1,3], rest: [7,6,2,3,5] }
break :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
break p = not <<< p # span

expand1 :: Matrix (Array Digit) -> Matrix (Array (Array Digit))
expand1 rs =
  let
    n = rs # counts >>> minimum >>> fromMaybe 0

    smallest cs = length cs == n

    { init: rows1, rest: rows2 } = break (any smallest) rs

    row = case uncons rows2 of
      Just { head: r, tail: _ } -> r
      Nothing -> []

    { init: row1, rest: row2 } = break smallest row

    choices2 = case uncons row2 of
      Just { head: cs, tail: _ } -> cs
      Nothing -> []
  in
    do
      c <- choices2
      pure (rows1 <> [ row1 <> [ c ] : row2 ] <> rows2)

complete :: Matrix (Array Digit) -> Boolean
complete = all (all single)

-- ok [[2],[3],[9],[8],[4],[1],[5],[6],[6]] => false
ok :: Array (Array Digit) -> Boolean
ok row = singletonsFromArr row # nodups

safe :: Matrix (Array Digit) -> Boolean
safe cm =
  all ok (rows cm)
    && all ok (cols cm)
    && all ok (boxs cm)

extract :: Matrix (Array Digit) -> Grid
extract = map concat

search :: Matrix (Array Digit) -> Array Grid
search cm = search' (fixedPoint prune cm)
  where
  search' pm
    | not (safe pm) = []
    | complete pm = [ extract pm ]
    | otherwise = expand1 pm # map search # concat

solve :: Grid -> Array Grid
-- solve = filter valid <<< expand <<< choices -- 5.1 Specification
-- solve = filter valid <<< expand <<< fixedPoint prune <<< choices -- 5.3 Pruning the matrix of choices
solve = search <<< choices -- 5.4 Expanding a single cell
