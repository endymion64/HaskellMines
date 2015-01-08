module MyBoard (Board(initialize, click, flag, won, lost), MyBoard(width, height, clickedCells, bombs), isMasked, isBomb, isFlagged, isClicked) where

import System.Random
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- To generate ranges of random tuples
instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)

  random gen1 =
   let (x, gen2) = random gen1
       (y, gen3) = random gen2 
    in ((x, y), gen3)
 
-- The show instance must be highly customized to display a board in ASCII
class Show b => Board b where
  -- Create a board from seed, dimension and first click
  initialize :: Int -> (Int, Int) -> (Int, Int) -> b
  -- Click a cell on the board (no effect if out-of-bounds)
  click :: (Int,Int) -> b -> b
  -- Flag a cell on the board (no effect if out-of-bounds)
  flag :: (Int,Int) -> b -> b
  -- Test if all the mines have been flagged and all the clean cells clicked 
  won :: b -> Bool
  -- Test if a mined cell has been clicked
  lost :: b -> Bool

type Cell = (Int, Int)

data MyBoard = Board { width :: Int
                     , height :: Int
                     , bombs :: (Set.Set Cell)
                     , maskedCells :: (Set.Set Cell)
                     , flaggedCells :: (Set.Set Cell)
                     , clickedCells :: (Map.Map Cell Int) -- value of map is the number of adjacent bombs
                     }

instance Show MyBoard where         
    show board
      | won board = printBoard board True
      | lost board = printBoard board True
      | otherwise =  printBoard board False

instance Board MyBoard where
    initialize seed (w, h) firstClick =
      let wNum = fromIntegral w -- to please the type inferencer for numbOfBombs
          hNum = fromIntegral h
          numOfBombs = round $ wNum * hNum * 0.20 -- 20% of the cells (rounded) will contain bombs
          notFirstClick x = not $ x == firstClick
          fun = (filter notFirstClick) . (take numOfBombs) . nub
          bombs = fun $ randomRs ((0,0), (w-1,h-1)) (mkStdGen seed) :: [(Int,Int)]
          fieldList = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
      in click firstClick 
               (Board w
                      h
                     (Set.fromList bombs)
                     (Set.fromList fieldList)
                     Set.empty
                     Map.empty)

    flag cell board@(Board w h bombs masked flags clicked) 
      | not $ inBounds cell board = board
      | isClicked cell board = board
      | otherwise = Board w h bombs newMasked newFlags clicked
         where newMasked = Set.delete cell masked
               newFlags = Set.insert cell flags

    click cell board
        | not $ inBounds cell board = board
        | isClicked cell board = board
        | otherwise =
            let allNeighbors = neighbors cell board
                adjBombs = length $ filter (\neighbor -> isBomb neighbor board) allNeighbors
                newMaskedCells = Set.delete cell (maskedCells board)
                newFlaggedCells = Set.delete cell (flaggedCells board)
                newClickedCells = Map.insert cell adjBombs (clickedCells board)
                newBoard = Board (width board) 
                                 (height board) 
                                 (bombs board)
                                 newMaskedCells
                                 newFlaggedCells
                                 newClickedCells
            in if ((not $ isBomb cell board) && (adjBombs == 0))
               then clicks (filter (\neighbor -> isMasked neighbor board) allNeighbors) newBoard
               else newBoard

    -- win = bombs subset flagged and empty mask
    won (Board _ _ bombs masked flags _) = (Set.size masked == 0) && (bombs == flags)

    -- lost == clicked subset bombs 
    lost (Board _ _ bombs _ _ clicked) =
        (Map.size clicked /= 0) &&
        ((Set.size $ Set.intersection (Set.fromList $ Map.keys clicked) bombs ) /= 0)

isMasked :: Cell -> MyBoard -> Bool
isMasked cell board = Set.member cell (maskedCells board)

isFlagged :: Cell -> MyBoard -> Bool
isFlagged cell board = Set.member cell (flaggedCells board)

isClicked :: Cell -> MyBoard -> Bool
isClicked cell board = Map.member cell (clickedCells board)

isBomb :: Cell -> MyBoard -> Bool
isBomb cell board = Set.member cell (bombs board)

inBounds :: Cell -> MyBoard -> Bool
inBounds (x,y) board =
    let w = width board
        h = height board
    in
        ((-1) < x) && 
        (x < w) && 
        ((-1) < y) && 
        (y < h)

-- Calculates the adjacent cells from the cell in a board
neighbors :: Cell -> MyBoard -> [Cell]
neighbors (x,y) b = [(adjX, adjY) | adjX <- [x-1..x+1], 
                                    adjY <- [y-1..y+1],
                                    not $ (adjX == x) && (adjY == y), -- the cell (x,y) isn't its own neighbor
                                    inBounds (adjX, adjY) b] 

-- Propagation function of clicking cells
clicks :: [Cell] -> MyBoard -> MyBoard
clicks [] board = board
clicks (cell:cells) board =
    clicks cells (click cell board)

-- The string representation of MyBoard

printBoard :: MyBoard -> Bool -> String
printBoard board  withBombs =
    "   " ++
    printColNumbers (width board) 0 ++
    "  " ++
    printBorder (width board) ++
    printRow (height board) board withBombs

printColNumbers :: Int -> Int -> String
printColNumbers 0 nr = "\n"
printColNumbers ctr nr = " " ++ show  nr ++ "  " ++ printColNumbers (ctr - 1) (nr + 1)

printBorder :: Int -> String
printBorder 0 = "+\n"
printBorder w = "+---" ++ printBorder (w-1)

printRow :: Int -> MyBoard -> Bool ->String
printRow 0 board withBombs = "\n"
printRow currRow board withBombs =
    show ((height board) - currRow) ++
    " " ++
    printCells currRow (width board) board withBombs ++
    "  " ++
    printBorder (width board)  ++
    printRow (currRow-1) board withBombs

printCells _ 0 _ _ = "|\n"
printCells currRow currColl board withBombs =
    let x = (width board) - currColl
        y = (height board) - currRow
        cell = (x,y)
        adjBombs = Map.findWithDefault 0 cell (clickedCells board)
        selectedCellFormat =  if withBombs && (isBomb cell board)
                              then bombCell 
                              else if (isFlagged cell board)
                                   then flaggedCell 
                                   else if (isMasked cell board)
                                        then maskedCell 
                                        else (printCell adjBombs)
    in  selectedCellFormat ++ printCells currRow (currColl-1) board withBombs

flaggedCell :: String
flaggedCell = "| F "

maskedCell::String
maskedCell = "|   "

bombCell :: String
bombCell = "| B "

printCell :: Int -> String
printCell b = "| " ++ show b ++ " "
