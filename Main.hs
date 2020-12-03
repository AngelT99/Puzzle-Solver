-- Please don't remove or alter these imports, they are required for marking.

import WirePuzzle  -- Types for describing a tile puzzle and its solution.
import Exp         -- Types for propositional expressions/formulas.
import Util        -- Helper functions for working with 2d lists.
import Hidden      -- Hidden helper functions for solving formulas and drawing
                   -- puzzles.

-- ------------
-- Helper types
-- ------------

-- We recommend you use these types in your solution, but you are free to
-- change them if you like.

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


-- -----------------------------------------------
--  Encode the problem as a propositional formula
-- -----------------------------------------------

encode :: Int -> [[(TileNumber, WireLayout)]] -> (GExp [Char])
encode n grid
      = setVariablesFalse (variablesSetFalse n grid) formula
        where formula = setVariablesTrue (variablesSetTrue n grid) (totalFormula n grid)

-- the whole logical formula
totalFormula :: Int -> [[(TileNumber, WireLayout)]] -> (GExp [Char])
totalFormula n grid
    = checkAllPairs grid `AND` checkAllWiresConstraints n grid

-- replace the variables with True/False based on the known information
setVariablesFalse :: [[Char]] -> (GExp [Char]) -> (GExp [Char])
setVariablesFalse wires formula
    = setVariable formula wires FALSE

setVariablesTrue :: [[Char]] -> (GExp [Char]) -> (GExp [Char])
setVariablesTrue wires formula
    = setVariable formula wires TRUE

-- known information
variablesSetFalse :: Int -> [[(TileNumber, WireLayout)]] -> [[Char]]
variablesSetFalse n grid
    = noWiresTiles ++ noWirePairs ++ twoStrTopEdge ++ twoStrBottomEdge ++ twoStrLeftEdge ++ twoStrRightEdge
    where noWiresTiles = foldr (++) [] [["U"++(show x), "D"++(show x), "L"++(show x), "R"++(show x)] | x <- return_NoWireTileNum grid]
          noWirePairs = foldr (++) [] [ pairsWithNoWire n grid x | x <- return_NoWireTileNum grid ]
          twoStrTopEdge = foldr (++) [] [["D"++(show x)] | x <- return_TopEdgeNum n (return_TwoStraightWireTileNum grid)]
          twoStrBottomEdge = foldr (++) [] [["U"++(show x)] | x <- return_BottomEdgeNum n (return_TwoStraightWireTileNum grid)]
          twoStrLeftEdge = foldr (++) [] [["R"++(show x)] | x <- return_LeftEdgeNum n (return_TwoStraightWireTileNum grid)]
          twoStrRightEdge = foldr (++) [] [["L"++(show x)] | x <- return_RightEdgeNum n (return_TwoStraightWireTileNum grid)]       

variablesSetTrue :: Int -> [[(TileNumber, WireLayout)]] -> [[Char]]
variablesSetTrue n grid
    = twoStrUDtiles ++ twoStrRLtiles ++ twoCorTopEdgetiles ++ twoCorBottomEdgetiles ++ twoCorLeftEdgetiles ++ twoCorRightEdgetiles
    where twoStrUDtiles = foldr (++) [] [["U"++(show x), "D"++(show x)] | x <- return_twoStrUDwireNum n (return_TwoStraightWireTileNum grid)]
          twoStrRLtiles = foldr (++) [] [["R"++(show x), "L"++(show x)] | x <- return_twoStrRLwireNum n (return_TwoStraightWireTileNum grid)]
          twoCorTopEdgetiles = foldr (++) [] [["D"++(show x)] | x <- return_TopEdgeNum n (return_TwoCornerWiresTileNum grid)]
          twoCorBottomEdgetiles = foldr (++) [] [["U"++(show x)] | x <- return_BottomEdgeNum n (return_TwoCornerWiresTileNum grid)]
          twoCorLeftEdgetiles = foldr (++) [] [["R"++(show x)] | x <- return_LeftEdgeNum n (return_TwoCornerWiresTileNum grid)]
          twoCorRightEdgetiles = foldr (++) [] [["L"++(show x)] | x <- return_RightEdgeNum n (return_TwoCornerWiresTileNum grid)]


-- noWire tiles' neighbours 
pairsWithNoWire :: Int -> [[(TileNumber, WireLayout)]] -> TileNumber -> [[Char]]
pairsWithNoWire n grid x
    | ifTopEdge n x && ifLeftEdge n x = ["U"++(show (x+n)), "L"++(show (x+1))]
    | ifTopEdge n x && ifRightEdge n x = ["U"++(show (x+n)), "R"++(show (x-1))]
    | ifBottomEdge n x && ifLeftEdge n x = ["D"++(show (x-n)), "L"++(show (x+1))]
    | ifBottomEdge n x && ifRightEdge n x = ["D"++(show (x-n)), "R"++(show (x-1))]
    | ifTopEdge n x = ["U"++(show (x+n)), "L"++(show (x+1)), "R"++(show (x-1))]
    | ifBottomEdge n x = ["D"++(show (x-n)), "L"++(show (x+1)), "R"++(show (x-1))]
    | ifLeftEdge n x = ["U"++(show (x+n)), "D"++(show (x-n)), "L"++(show (x+1))]
    | ifRightEdge n x = ["U"++(show (x+n)), "D"++(show (x-n)), "R"++(show (x-1))]
    | otherwise = ["U"++(show (x+n)), "D"++(show (x-n)), "L"++(show (x+1)), "R"++(show (x-1))]

 
ifPairs :: Int -> [[(TileNumber, WireLayout)]] -> TileNumber -> TileNumber -> Bool
ifPairs n grid a b
    | ((a, b) `elem` allPairs) || ((b, a) `elem` allPairs)  = True
    | otherwise = False
     where allPairs = return_allPairsTileNum grid
                        
           
return_twoStrUDwireNum :: Int -> [TileNumber]-> [TileNumber]
return_twoStrUDwireNum n tiles
    = (filter (ifLeftEdge n) tiles) ++ (filter (ifRightEdge n) tiles)
    
return_twoStrRLwireNum :: Int -> [TileNumber]-> [TileNumber]
return_twoStrRLwireNum n tiles
    = (filter (ifTopEdge n) tiles) ++ (filter (ifBottomEdge n) tiles)

return_TopEdgeNum :: Int -> [TileNumber]-> [TileNumber]
return_TopEdgeNum n tiles
    = (filter (ifTopEdge n) tiles)

return_BottomEdgeNum :: Int -> [TileNumber]-> [TileNumber]
return_BottomEdgeNum n tiles
    = (filter (ifBottomEdge n) tiles)

return_LeftEdgeNum :: Int -> [TileNumber]-> [TileNumber]
return_LeftEdgeNum n tiles
    = (filter (ifLeftEdge n) tiles)
    
return_RightEdgeNum :: Int -> [TileNumber]-> [TileNumber]
return_RightEdgeNum n tiles
    = (filter (ifRightEdge n) tiles)


-- replace variables with true/false in a formula
--                       Formula    Variable  Value    Output
setVariable :: (Eq a) => (GExp a) -> [a] -> (GExp a) -> (GExp a)
setVariable FALSE varnames value = FALSE
setVariable TRUE varnames value = TRUE
setVariable (VAR x) varnames value = if x `elem` varnames then value else (VAR x)
setVariable (NOT x) varnames value = NOT (setVariable x varnames value)
setVariable (AND x y) varnames value = AND (setVariable x varnames value) (setVariable y varnames value)
setVariable (OR x y) varnames value = OR (setVariable x varnames value) (setVariable y varnames value)


-- For all hPairs and vPairs, check if they are connected to each other
return_HorizPairsTileNum :: [[(TileNumber, WireLayout)]] -> [(TileNumber, TileNumber)]
return_HorizPairsTileNum grid
    = [(tile1, tile2) | ((tile1, wirelayout1),(tile2,wirelayout2)) <- hPairs grid]
    
return_VerticalPairsTileNum :: [[(TileNumber, WireLayout)]] -> [(TileNumber, TileNumber)]
return_VerticalPairsTileNum grid
    = [(tile1, tile2) | ((tile1, wirelayout1),(tile2,wirelayout2)) <- vPairs grid]
 
return_allPairsTileNum :: [[(TileNumber, WireLayout)]] -> [(TileNumber, TileNumber)]
return_allPairsTileNum grid
    = return_HorizPairsTileNum grid ++ return_VerticalPairsTileNum grid
 
checkHorizPair :: [(TileNumber, TileNumber)] -> (GExp [Char])
checkHorizPair tileList
    = foldr AND TRUE formula
     where formula = [ ((VAR ("R"++(show x)) `AND` VAR ("L"++(show y))) `OR` ((NOT (VAR ("R"++(show x)))) `AND` (NOT (VAR ("L"++(show y)))))) | (x,y) <- tileList ]

checkVerticalPair :: [(TileNumber, TileNumber)] -> (GExp [Char])
checkVerticalPair tileList
    = foldr AND TRUE formula
      where formula = [ ((VAR ("D"++(show x)) `AND` VAR ("U"++(show y))) `OR` ((NOT (VAR ("D"++(show x)))) `AND` (NOT (VAR ("U"++(show y)))))) | (x,y) <- tileList ]

checkAllPairs :: [[(TileNumber, WireLayout)]] -> (GExp [Char])
checkAllPairs grid
    = checkHorizPair horiz_tiles `AND` checkVerticalPair vertical_tiles
      where horiz_tiles = return_HorizPairsTileNum grid
            vertical_tiles = return_VerticalPairsTileNum grid
           
whichWire ::WireLayout -> (TileNumber, WireLayout) -> Bool
whichWire target_wire (tile, wire)
    | target_wire == wire = True
    | otherwise = False

-- check if the tile is at the edge
ifTopEdge :: Int -> TileNumber -> Bool
ifTopEdge n tile
    | tile < n = True
    | otherwise = False
 
ifBottomEdge :: Int -> TileNumber -> Bool
ifBottomEdge n tile
    | n*(n-1) <= tile && tile <= n*n-1 = True
    | otherwise = False
    
ifLeftEdge :: Int -> TileNumber -> Bool
ifLeftEdge n tile
    | (tile `mod` n) == 0 = True
    | otherwise = False
    
ifRightEdge :: Int -> TileNumber -> Bool
ifRightEdge n tile
    | ((tile + 1) `mod` n) == 0 = True
    | otherwise = False
    
------------------------------------------------
-- List out all the constrains for NoWires:
return_NoWireTileNum :: [[(TileNumber, WireLayout)]] -> [TileNumber]
return_NoWireTileNum grid
   = [ tile | (tile, wire) <- filteredWiresTuples]
      where filteredWiresTuples = filter (whichWire NoWires) [(tile, wirelayout) | (tile, wirelayout) <- flatten grid ]
   
    
checkNoWire :: [TileNumber] -> (GExp [Char])
checkNoWire tiles
    = foldr AND TRUE formula
       where formula = [(((NOT (VAR ("U"++(show x)))) `AND` (NOT (VAR ("D"++(show x))))) `AND` ((NOT (VAR ("L"++(show x)))) `AND` (NOT (VAR ("R"++(show x)))))) | x <- tiles]

noWiresConstraints :: [[(TileNumber, WireLayout)]] -> (GExp [Char])
noWiresConstraints grid
    = checkNoWire (return_NoWireTileNum grid)

-------------------------------------------------
-- List out all the constrains for OneWire:
return_OneWireTileNum :: [[(TileNumber, WireLayout)]] -> [TileNumber]
return_OneWireTileNum grid
   = [ tile | (tile, wire) <- filteredWiresTuples]
      where filteredWiresTuples = filter (whichWire OneWire) [(tile, wirelayout) | (tile, wirelayout) <- flatten grid ]

checkOneWire :: Int -> [TileNumber] -> (GExp [Char])
checkOneWire n tiles
    = foldr AND TRUE formula
      where formula = oneWireCondition n tiles
     
oneWireFormula :: TileNumber -> (GExp [Char])
oneWireFormula x
    = ((part1 `OR` part2) `OR` (part3 `OR` part4))
      where part1 = (((VAR ("U"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) `AND` ((NOT (VAR ("D"++(show x)))) `AND` (NOT (VAR ("L"++(show x))))))
            part2 = (((VAR ("D"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) `AND` ((NOT (VAR ("U"++(show x)))) `AND` (NOT (VAR ("L"++(show x))))))
            part3 = (((VAR ("L"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) `AND` ((NOT (VAR ("D"++(show x)))) `AND` (NOT (VAR ("U"++(show x))))))
            part4 = (((VAR ("R"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) `AND` ((NOT (VAR ("D"++(show x)))) `AND` (NOT (VAR ("L"++(show x))))))

oneWireTopFormula :: TileNumber -> (GExp [Char])
oneWireTopFormula x
    = ((part1 `OR` part2) `OR` (part3))
      where part1 = (((VAR ("D"++(show x))) `AND` (NOT (VAR ("L"++(show x))))) `AND` (NOT (VAR ("R"++(show x)))))
            part2 = (((VAR ("R"++(show x))) `AND` (NOT (VAR ("D"++(show x))))) `AND` (NOT (VAR ("L"++(show x)))))
            part3 = (((VAR ("L"++(show x))) `AND` (NOT (VAR ("D"++(show x))))) `AND` (NOT (VAR ("R"++(show x)))))

oneWireBottomFormula :: TileNumber -> (GExp [Char])
oneWireBottomFormula x
    = ((part1 `OR` part2) `OR` (part3))
      where part1 = (((VAR ("U"++(show x))) `AND` (NOT (VAR ("L"++(show x))))) `AND` (NOT (VAR ("R"++(show x)))))
            part2 = (((VAR ("R"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) `AND` (NOT (VAR ("L"++(show x)))))
            part3 = (((VAR ("L"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) `AND` (NOT (VAR ("R"++(show x)))))

oneWireLeftFormula :: TileNumber -> (GExp [Char])
oneWireLeftFormula x
    = ((part1 `OR` part2) `OR` (part3))
      where part1 = (((VAR ("R"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) `AND` (NOT (VAR ("D"++(show x)))))
            part2 = (((VAR ("D"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) `AND` (NOT (VAR ("U"++(show x)))))
            part3 = (((VAR ("U"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) `AND` (NOT (VAR ("D"++(show x)))))

oneWireRightFormula :: TileNumber -> (GExp [Char])
oneWireRightFormula x
    = ((part1 `OR` part2) `OR` (part3))
      where part1 = (((VAR ("L"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) `AND` (NOT (VAR ("D"++(show x)))))
            part2 = (((VAR ("D"++(show x))) `AND` (NOT (VAR ("L"++(show x))))) `AND` (NOT (VAR ("U"++(show x)))))
            part3 = (((VAR ("U"++(show x))) `AND` (NOT (VAR ("L"++(show x))))) `AND` (NOT (VAR ("D"++(show x)))))

oneWireLeftTopCorFormula :: TileNumber -> (GExp [Char])
oneWireLeftTopCorFormula x
    = part1 `OR` part2
      where part1 = ((VAR ("R"++(show x))) `AND` (NOT (VAR ("D"++(show x))))) 
            part2 = ((VAR ("D"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) 
            
oneWireRightTopCorFormula :: TileNumber -> (GExp [Char])
oneWireRightTopCorFormula x
    = part1 `OR` part2
      where part1 = ((VAR ("L"++(show x))) `AND` (NOT (VAR ("D"++(show x))))) 
            part2 = ((VAR ("D"++(show x))) `AND` (NOT (VAR ("L"++(show x)))))   
            
oneWireLeftBottomCorFormula :: TileNumber -> (GExp [Char])
oneWireLeftBottomCorFormula x
    = part1 `OR` part2
      where part1 = ((VAR ("U"++(show x))) `AND` (NOT (VAR ("R"++(show x))))) 
            part2 = ((VAR ("R"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) 

oneWireRightBottomCorFormula :: TileNumber -> (GExp [Char])
oneWireRightBottomCorFormula x
    = part1 `OR` part2
      where part1 = ((VAR ("L"++(show x))) `AND` (NOT (VAR ("U"++(show x))))) 
            part2 = ((VAR ("U"++(show x))) `AND` (NOT (VAR ("L"++(show x))))) 

oneWireCondition :: Int -> [TileNumber] -> [(GExp [Char])]
oneWireCondition n tiles
    | tiles == [] = []
    | (ifTopEdge n x) && (ifLeftEdge n x) = [oneWireLeftTopCorFormula x] ++ recurs
    | (ifTopEdge n x) && (ifRightEdge n x) = [oneWireRightTopCorFormula x] ++ recurs
    | (ifBottomEdge n x) && (ifLeftEdge n x) = [oneWireLeftBottomCorFormula x] ++ recurs
    | (ifBottomEdge n x) && (ifRightEdge n x) = [oneWireRightBottomCorFormula x] ++ recurs
    | ifTopEdge n x = [oneWireTopFormula x] ++ recurs
    | ifBottomEdge n x = [oneWireBottomFormula x] ++ recurs
    | ifLeftEdge n x = [oneWireLeftFormula x] ++ recurs
    | ifRightEdge n x = [oneWireRightFormula x] ++ recurs
    | otherwise = [oneWireFormula x] ++ recurs
    where recurs = oneWireCondition n (drop 1 tiles)
          x = tiles !! 0


oneWiresConstraints :: Int -> [[(TileNumber, WireLayout)]] -> (GExp [Char])
oneWiresConstraints n grid
    = checkOneWire n (return_OneWireTileNum grid)

------------------------------------------------
-- List out all the constrains for TwoStraightWires:
return_TwoStraightWireTileNum :: [[(TileNumber, WireLayout)]] -> [TileNumber]
return_TwoStraightWireTileNum grid
   = [ tile | (tile, wire) <- filteredWiresTuples]
      where filteredWiresTuples = filter (whichWire TwoStraightWires) [(tile, wirelayout) | (tile, wirelayout) <- flatten grid ]
   
    
checkTwoStraightWires :: Int -> [TileNumber] -> (GExp [Char])
checkTwoStraightWires n tiles
    = foldr AND TRUE formula
        where formula = twoStrLineCondition n tiles


twoStrLineFormula :: TileNumber -> (GExp [Char])
twoStrLineFormula x
    = (leftHandside `OR` rightHandside)
      where leftHandside = (((VAR ("U"++(show x))) `AND` (VAR ("D"++(show x)))) `AND` ((NOT (VAR ("L"++(show x)))) `AND` (NOT (VAR ("R"++(show x))))))
            rightHandside = (((VAR ("L"++(show x))) `AND` (VAR ("R"++(show x)))) `AND` ((NOT (VAR ("U"++(show x)))) `AND` (NOT (VAR ("D"++(show x))))))

twoStrLineCondition :: Int -> [TileNumber] -> [(GExp [Char])]
twoStrLineCondition n tiles
    | tiles == [] = []
    | ifTopEdge n x = [(VAR ("L"++(show x))) `AND` (VAR ("R"++(show x))) `AND` (NOT (VAR ("D"++(show x))))] ++ recurs
    | ifBottomEdge n x = [(VAR ("L"++(show x))) `AND` (VAR ("R"++(show x))) `AND` (NOT (VAR ("U"++(show x))))] ++ recurs
    | ifLeftEdge n x = [(VAR ("U"++(show x))) `AND` (VAR ("D"++(show x))) `AND` (NOT (VAR ("R"++(show x))))] ++ recurs
    | ifRightEdge n x = [(VAR ("U"++(show x))) `AND` (VAR ("D"++(show x))) `AND` (NOT (VAR ("L"++(show x))))] ++ recurs
    | otherwise = [twoStrLineFormula x] ++ recurs
    where recurs = twoStrLineCondition n (drop 1 tiles)
          x = tiles !! 0

twoStraightWiresConstraints :: Int -> [[(TileNumber, WireLayout)]] -> (GExp [Char])
twoStraightWiresConstraints n grid
    = checkTwoStraightWires n (return_TwoStraightWireTileNum grid)
    
   
------------------------------------------------
-- List out all the constrains for TwoCornerWires:
return_TwoCornerWiresTileNum :: [[(TileNumber, WireLayout)]] -> [TileNumber]
return_TwoCornerWiresTileNum grid
   = [ tile | (tile, wire) <- filteredWiresTuples]
      where filteredWiresTuples = filter (whichWire TwoCornerWires) [(tile, wirelayout) | (tile, wirelayout) <- flatten grid ]
   
    
checkTwoCornerWires :: Int -> [TileNumber] -> (GExp [Char])
checkTwoCornerWires n tiles
    = foldr AND TRUE formula
    -- where formula = [(twoCorLineFormula x) | x <- tiles]
      where formula = twoCorLineCondition n tiles
     
twoCorLineFormula :: TileNumber -> (GExp [Char])
twoCorLineFormula x
    = ((part1 `OR` part2) `OR` (part3 `OR` part4))
      where part1 = (((VAR ("U"++(show x))) `AND` (VAR ("R"++(show x)))) `AND` ((NOT (VAR ("D"++(show x)))) `AND` (NOT (VAR ("L"++(show x))))))
            part2 = (((VAR ("D"++(show x))) `AND` (VAR ("L"++(show x)))) `AND` ((NOT (VAR ("U"++(show x)))) `AND` (NOT (VAR ("R"++(show x))))))
            part3 = (((VAR ("R"++(show x))) `AND` (VAR ("D"++(show x)))) `AND` ((NOT (VAR ("L"++(show x)))) `AND` (NOT (VAR ("U"++(show x))))))
            part4 = (((VAR ("U"++(show x))) `AND` (VAR ("L"++(show x)))) `AND` ((NOT (VAR ("R"++(show x)))) `AND` (NOT (VAR ("D"++(show x))))))

twoCorLineTopFormula :: TileNumber -> (GExp [Char])
twoCorLineTopFormula x
    = part1 `OR` part2
      where part1 = (((VAR ("D"++(show x))) `AND` (VAR ("L"++(show x)))) `AND` (NOT (VAR ("R"++(show x)))))
            part2 = (((VAR ("D"++(show x))) `AND` (VAR ("R"++(show x)))) `AND` (NOT (VAR ("L"++(show x)))))
            
twoCorLineBottomFormula :: TileNumber -> (GExp [Char])
twoCorLineBottomFormula x
    = part1 `OR` part2
      where part1 = (((VAR ("U"++(show x))) `AND` (VAR ("R"++(show x)))) `AND` (NOT (VAR ("L"++(show x)))))
            part2 = (((VAR ("U"++(show x))) `AND` (VAR ("L"++(show x)))) `AND` (NOT (VAR ("R"++(show x)))))
            
twoCorLineLeftFormula :: TileNumber -> (GExp [Char])
twoCorLineLeftFormula x
    = part1 `OR` part2
      where part1 = (((VAR ("U"++(show x))) `AND` (VAR ("R"++(show x)))) `AND` (NOT (VAR ("D"++(show x)))))
            part2 = (((VAR ("R"++(show x))) `AND` (VAR ("D"++(show x)))) `AND` (NOT (VAR ("U"++(show x)))))

twoCorLineRightFormula :: TileNumber -> (GExp [Char])
twoCorLineRightFormula x
    = part1 `OR` part2
      where part1 = (((VAR ("D"++(show x))) `AND` (VAR ("L"++(show x)))) `AND` (NOT (VAR ("U"++(show x)))))
            part2 = (((VAR ("U"++(show x))) `AND` (VAR ("L"++(show x)))) `AND` (NOT (VAR ("D"++(show x)))))

twoCorLineCondition :: Int -> [TileNumber] -> [(GExp [Char])]
twoCorLineCondition n tiles
    | tiles == [] = []
    | (ifTopEdge n x) && (ifLeftEdge n x) = [((VAR ("R"++(show x))) `AND` (VAR ("D"++(show x))))] ++ recurs
    | (ifTopEdge n x) && (ifRightEdge n x) = [((VAR ("L"++(show x))) `AND` (VAR ("D"++(show x))))] ++ recurs
    | (ifBottomEdge n x) && (ifLeftEdge n x) = [((VAR ("R"++(show x))) `AND` (VAR ("U"++(show x))))] ++ recurs
    | (ifBottomEdge n x) && (ifRightEdge n x) = [((VAR ("L"++(show x))) `AND` (VAR ("U"++(show x))))] ++ recurs
    | ifTopEdge n x = [twoCorLineTopFormula x] ++ recurs
    | ifBottomEdge n x = [twoCorLineBottomFormula x] ++ recurs
    | ifLeftEdge n x = [twoCorLineLeftFormula x] ++ recurs
    | ifRightEdge n x = [twoCorLineRightFormula x] ++ recurs
    | otherwise = [twoCorLineFormula x] ++ recurs
    where recurs = twoCorLineCondition n (drop 1 tiles)
          x = tiles !! 0
          

twoCornerWiresConstraints :: Int -> [[(TileNumber, WireLayout)]] -> (GExp [Char])
twoCornerWiresConstraints n grid
    = checkTwoCornerWires n (return_TwoCornerWiresTileNum grid)
    
    
-- checking all four wirelayouts' constraints   
checkAllWiresConstraints :: Int -> [[(TileNumber, WireLayout)]] -> (GExp [Char])
checkAllWiresConstraints n grid
    = part1 `AND` part2 `AND` part3 `AND` part4
      where part1 = noWiresConstraints grid
            part2 = oneWiresConstraints n grid
            part3 = twoStraightWiresConstraints n grid
            part4 = twoCornerWiresConstraints n grid
          
-- ----------------------------------------------------
--  Decode the formula's solution as a puzzle solution
-- ----------------------------------------------------

-- Implement this function, or replace with your own approach.

--my Decode

ifUpNoWireNearby :: [[(TileNumber, WireLayout)]] -> Int -> TileNumber -> Bool
ifUpNoWireNearby grid n x
    | ((not (ifTopEdge n x)) && (x-n) `elem` noWireTiles) = True
    | otherwise = False
    where noWireTiles = return_NoWireTileNum grid
    
ifDownNoWireNearby :: [[(TileNumber, WireLayout)]] -> Int -> TileNumber -> Bool
ifDownNoWireNearby grid n x
    | ((not (ifBottomEdge n x)) && (x+n) `elem` noWireTiles) = True
    | otherwise = False
    where noWireTiles = return_NoWireTileNum grid
    
ifLeftNoWireNearby :: [[(TileNumber, WireLayout)]] -> Int -> TileNumber -> Bool
ifLeftNoWireNearby grid n x
    | ((not (ifLeftEdge n x)) && (x-1) `elem` noWireTiles) = True
    | otherwise = False
    where noWireTiles = return_NoWireTileNum grid
    
ifRightNoWireNearby :: [[(TileNumber, WireLayout)]] -> Int -> TileNumber -> Bool
ifRightNoWireNearby grid n x
    | ((not (ifRightEdge n x)) && (x+1) `elem` noWireTiles) = True
    | otherwise = False
    where noWireTiles = return_NoWireTileNum grid
    


decode :: [[(TileNumber, WireLayout)]] -> ([Char] -> Bool) -> [[Orientation]]
decode grid asg
  = [ [ decodeTile n grid tile asg | tile <- row ] | row <- grid ]
      where n = length grid

-- We have given you a starter implementation. The list comprehension above
-- builds the solution grid by calling `decodeTile` for each tile in the
-- input grid. You can start from here, or replace with your own approach.

decodeTile :: Int ->[[(TileNumber, WireLayout)]] -> (TileNumber, WireLayout) -> ([Char] -> Bool) -> Orientation
decodeTile n grid (t, NoWires) asg
  = None
  
decodeTile n grid (t, OneWire) asg
  | not (ifTopEdge n t) && not (ifUpNoWireNearby grid n t) && asg ("U"++(show t)) = U
  | not (ifBottomEdge n t) && not (ifDownNoWireNearby grid n t) && asg ("D"++(show t)) = D
  | not (ifLeftEdge n t) && not (ifLeftNoWireNearby grid n t) && asg ("L"++(show t)) = L
  | not (ifRightEdge n t) && not (ifRightNoWireNearby grid n t) && asg ("R"++(show t)) = R
  | otherwise = R
    
  
decodeTile n grid (t, TwoStraightWires) asg
  | (ifTopEdge n t) || (ifBottomEdge n t) = RL
  | (ifRightEdge n t) || (ifLeftEdge n t) = UD
  | (ifDownNoWireNearby grid n t) || (ifUpNoWireNearby grid n t) = RL
  | (ifLeftNoWireNearby grid n t) || (ifRightNoWireNearby grid n t) = UD
  | not (ifTopEdge n t) && not (ifBottomEdge n t) && asg ("U"++(show t)) && asg ("D"++(show t)) = UD
  | not (ifRightEdge n t) && not (ifLeftEdge n t) && asg ("L"++(show t)) && asg ("R"++(show t)) = RL
  | otherwise = RL
  
decodeTile n grid (t, TwoCornerWires) asg
  -- corners
  | ((ifTopEdge n t) && (ifLeftEdge n t)) || ((ifUpNoWireNearby grid n t) && (ifLeftNoWireNearby grid n t)) = RD
  | ((ifTopEdge n t) && (ifRightEdge n t)) || ((ifUpNoWireNearby grid n t) && (ifRightNoWireNearby grid n t)) = DL
  | ((ifBottomEdge n t) && (ifLeftEdge n t)) || ((ifDownNoWireNearby grid n t) && (ifLeftNoWireNearby grid n t)) = UR
  | ((ifBottomEdge n t) && (ifRightEdge n t)) || ((ifDownNoWireNearby grid n t) && (ifLeftNoWireNearby grid n t)) = UL
  
  -- edges
  | (ifTopEdge n t) && not (ifLeftNoWireNearby grid n t) && asg ("L"++(show t)) = DL
  | (ifTopEdge n t) && not (ifRightNoWireNearby grid n t) && asg ("R"++(show t)) = RD
  | (ifBottomEdge n t) && not (ifLeftNoWireNearby grid n t) && asg ("L"++(show t)) = UL
  | (ifBottomEdge n t) && not (ifRightNoWireNearby grid n t) && asg ("R"++(show t)) = UR
  | (ifLeftEdge n t) && not (ifUpNoWireNearby grid n t) && asg ("U"++(show t)) = UR
  | (ifLeftEdge n t) && not (ifDownNoWireNearby grid n t) && asg ("D"++(show t)) = RD
  | (ifRightEdge n t) && not (ifUpNoWireNearby grid n t) && asg ("U"++(show t)) = UL
  | (ifRightEdge n t) && not (ifDownNoWireNearby grid n t) && asg ("D"++(show t)) = DL
  
  | not (ifRightEdge n t) && not (ifTopEdge n t) && not (ifUpNoWireNearby grid n t) && not (ifRightNoWireNearby grid n t) && asg ("U"++(show t)) && asg ("R"++(show t)) = UR
  | not (ifLeftEdge n t) && not (ifBottomEdge n t) && not (ifDownNoWireNearby grid n t) && not (ifLeftNoWireNearby grid n t) && asg ("D"++(show t)) && asg ("L"++(show t)) = DL
  | not (ifRightEdge n t) && not (ifBottomEdge n t) && not (ifRightNoWireNearby grid n t) && not (ifDownNoWireNearby grid n t) && asg ("R"++(show t)) && asg ("D"++(show t)) = RD
  | not (ifLeftEdge n t) && not (ifTopEdge n t) && not (ifUpNoWireNearby grid n t) && not (ifLeftNoWireNearby grid n t) && asg ("U"++(show t)) && asg ("L"++(show t)) = UL
  | otherwise = UL
