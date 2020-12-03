import WirePuzzle
import Exp
import Util
import Hidden

-- ------------
-- Helper types
-- ------------

type VarName
  = (TileNumber, Int)

type Exp
  = GExp VarName


-- ---------------------------------------
-- Encode, solve with a SAT solver, decode
-- ---------------------------------------

-- solvePuzzle
-- Find and return a grid of orientations that will
-- connect a given puzzle grid of wire tiles.
solvePuzzle :: [[(TileNumber, WireLayout)]] -> [[Orientation]]
solvePuzzle grid
  = decode grid (solveExp (encode (length grid) grid))

-- We use the following variable allocation. Note that many alternatives
-- are possible, but this one involves a small number of variables per tile.
--
-- * For each NoWires tile: no variables
--
-- * For each OneWire tile: two variables V0 and V1
--
--   V1 | V0 | Orientation
--   =====================
--    F |  F | Up
--    F |  T | Right
--    T |  F | Down
--    T |  T | Left
--
-- * For each TwoStraightWires tile: one variable V0
--
--   V0 | Orientation
--   ================
--    F | UpDown
--    T | RightLeft
--
--  * For each TwoCornerWires tile: two variables V0 and V1, which control
--    the vertical wire and the horizontal wire respectively
--
--   V0 | Orientation
--   ================
--    F | Up   (UpRight   or UpLeft)
--    T | Down (RightDown or DownLeft)
--
--   V1 | Orientation
--   ================
--    F | Right (UpRight or RightDown)
--    T | Left  (UpLeft  or DownLeft)
--    
-- Note that with this encoding there is no need to enforce that each tile
-- must be in exactly one configuration.


-- -----------------------------------------------
--  Encode the problem as a propositional formula
-- -----------------------------------------------

-- encode
-- Encode problem specification as a propositional formula.
encode :: Int -> [[(TileNumber, WireLayout)]] -> Exp
encode n grid
  = conjoin $ map conjoin
    -- each adjacent pair of tiles must either both or neither point inwards
    [ [ (t1 `points` R) `BIIM` (t2 `points` L) | (t1, t2) <- hPairs grid ]
    , [ (t1 `points` D) `BIIM` (t2 `points` U) | (t1, t2) <- vPairs grid ]
    -- each tile near an edge must not point towards the edge
    , [ NOT (t `points` U) | t <- row 0 grid ]
    , [ NOT (t `points` R) | t <- col (n-1) grid ]
    , [ NOT (t `points` D) | t <- row (n-1) grid ]
    , [ NOT (t `points` L) | t <- col 0 grid ]
    ]

-- points
-- Build a position that is true if and only if the given tile
-- has a wire pointing in the given direction (U, R, D, or L).
points :: (TileNumber, WireLayout) -> Orientation -> Exp
points (_, NoWires) _
  = FALSE
points (t, OneWire) d
  | d == U = AND (NOT (VAR (t, 0))) (NOT (VAR (t, 1)))
  | d == R = AND (NOT (VAR (t, 0)))      (VAR (t, 1))
  | d == D = AND      (VAR (t, 0))  (NOT (VAR (t, 1)))
  | d == L = AND      (VAR (t, 0))       (VAR (t, 1))
points (t, TwoStraightWires) d
  | d == U || d == D = NOT (VAR (t, 0))
  | d == R || d == L =      VAR (t, 0)
points (t, TwoCornerWires) d
  | d == U = NOT (VAR (t, 0))
  | d == R =      VAR (t, 1)
  | d == D =      VAR (t, 0)
  | d == L = NOT (VAR (t, 1))

-- conjoin
-- Convert from a list of expressions to a single expression
-- representing their conjunction.
-- For example:
--   conjoin [e, f, g, h] = e `AND` (f `AND` (g `AND` h))
conjoin :: [Exp] -> Exp
conjoin []
  = TRUE
conjoin xs
  = foldr1 AND xs


-- ----------------------------------------------------
--  Decode the formula's solution as a puzzle solution
-- ----------------------------------------------------

-- decode
-- Decode formula's satisfying assignment as tile orientations.
decode :: [[(TileNumber, WireLayout)]] -> (VarName -> Bool) -> [[Orientation]]
decode grid asg
  = [ [ decodeTile tile asg | tile <- row ] | row <- grid ]

-- decodeTile
-- Decode orientation for a single tile's variables.
decodeTile :: (TileNumber, WireLayout) -> (VarName -> Bool) -> Orientation
decodeTile (_, NoWires) _
  = None
decodeTile (t, OneWire) asg
  | asg (t, 0) == False && asg (t, 1) == False = U
  | asg (t, 0) == False && asg (t, 1) == True  = R
  | asg (t, 0) == True  && asg (t, 1) == False = D
  | asg (t, 0) == True  && asg (t, 1) == True  = L
decodeTile (t, TwoStraightWires) asg
  | asg (t, 0) == False = UD
  | asg (t, 0) == True  = RL
decodeTile (t, TwoCornerWires) asg
  | asg (t, 0) == False && asg (t, 1) == False = UL
  | asg (t, 0) == False && asg (t, 1) == True  = UR
  | asg (t, 0) == True  && asg (t, 1) == False = DL
  | asg (t, 0) == True  && asg (t, 1) == True  = RD
