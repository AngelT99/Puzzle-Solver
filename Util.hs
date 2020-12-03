module Util
where

import Data.List (transpose)

-- row
-- Return the `i`th row of `grid`. Count from 0.
-- For example:
--   > row 1 [[1,2,3], [4,5,6]]
--   [4,5,6]
row :: Int -> [[a]] -> [a]
row i grid
  = grid !! i

-- col
-- Return the `i`th column of `grid`. Count from 0.
-- For example:
--   > col 1 [[1,2,3], [4,5,6]]
--   [2,5]
col :: Int -> [[a]] -> [a]
col i grid
  = (transpose grid) !! i

-- flatten
-- Return all the elements of `grid` in a single list.
-- For example:
--   > flatten [[1,2,3], [4,5,6]]
--   [1,2,3,4,5,6]
flatten :: [[a]] -> [a]
flatten grid
  = concat grid

-- hPairs
-- Return all of the horizontally adjacent pairs of items in `grid`.
-- For example:
--   > hPairs [[1,2,3], [4,5,6]]
--   [ (1,2), (2,3), (4,5), (5,6) ]
hPairs :: [[a]] -> [(a, a)]
hPairs grid
  = concatMap pairs grid

-- vPairs
-- Return all of the vertically adjacent pairs of items in `grid`.
-- For example:
--   > vPairs [[1,2,3], [4,5,6]]
--   [ (1,4), (2,5), (3,6) ]
vPairs :: [[a]] -> [(a, a)]
vPairs grid
  = concatMap pairs (transpose grid)

-- pairs
-- Return all adjacent pairs of items from `list`.
-- For example:
--   > pairs [1,2,3,4]
--   [ (1,2), (2,3), (3,4)
pairs :: [a] -> [(a, a)]
pairs list
  = zip list (tail list)
