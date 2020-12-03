module WirePuzzle
where

-- ------------------------------------------
-- Types representing the wire puzzle problem
-- ------------------------------------------

-- Each tile has one of four possible wire layouts:
--  NoWires:            OneWire:
--        ┌───┐              ┌───┐
--        │ O │              │ O━┥  
--        └───┘              └───┘
--  TwoStraightWires:   TwoCornerWires:
--        ┌───┐              ┌───┐
--        ┝━O━┥              │ O━┥
--        └───┘              └─┸─┘
data WireLayout
  = NoWires
  | OneWire
  | TwoStraightWires
  | TwoCornerWires
  deriving (Eq, Ord, Show)

-- Each tile has one of several orientations,
-- depending on the wire layout.
-- For example, OneWire tiles can point Up (U),
-- Right (R), Down (D), or Left (L):
--     U        R        D        L
--   ┌─┰─┐    ┌───┐    ┌───┐    ┌───┐
--   │ O │    │ O━┥    │ O │    ┝━O │
--   └───┘    └───┘    └─┸─┘    └───┘
data Orientation
  = None              -- for NoWires tiles
  | U  | R  | D  | L  -- for OneWire tiles
  | UD | RL           -- for TwoStraightWires tiles
  | UR | RD | DL | UL -- for TwoCornerWires tiles
  deriving (Eq, Ord, Show)

-- Each tile also has its own number, based on
-- its position in the puzzle grid:
--               ┌───┬───┐
--               │ 0 │ 1 │
--               ├─╂─┼─╂─┤
--               │ 2━┿━3 │
--               └───┴───┘
type TileNumber
  = Int
