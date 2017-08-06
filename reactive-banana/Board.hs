module Board
    ( Board(..), BoardAction(..)
    , canMove, initialBoard, updateBoard
    ) where

import Data.List (transpose)

-- Board
type Cells = [[Int]]
data Board = Board
  { _cells   :: Cells
  , _score   :: Int
  }

blankCell = 0

-- Board actions
data BoardAction = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show

-- Initial board
initialBoard :: Int -> Board
initialBoard size = Board cells 0
  where cells = (replace2 (0, 3) 1 $ replace2 (1, 2) 2 $ replace2 (0, 0) 1 emptyCells)
        emptyCells = replicate size $ replicate size blankCell

boardSize :: Board -> Int
boardSize = length . _cells

canMove :: Board -> Bool
canMove board = any canSlide [MoveLeft, MoveRight, MoveUp, MoveDown]
  where cells = _cells board
        canSlide act = (fst $ slideTo act cells) /= cells

updateBoard :: Int -> BoardAction -> Board -> Board
updateBoard r act board
  | canSlide   = Board (spawnPanel r act slided) (_score board + point)
  | otherwise  = board
  where canSlide = cells /= slided
        cells = _cells board
        (slided, point) = slideTo act cells

spawnPanel :: Int -> BoardAction -> Cells -> Cells
spawnPanel rnd act cells = replace2 targetPos panel cells
  where blankCells = filter ((== blankCell) . fst) cellsWithPos
        targetIndex = rnd `mod` length blankCells
        targetPos = snd (blankCells !! targetIndex)
        panel = [1, 1, 1, 2] !! ((rnd `div` length blankCells) `mod` 4)
        cellsWithPos = concat $ zipWith (\row r -> zipWith (\cell c -> (cell, (r, c))) row [0..]) cells [0..]

slideTo :: BoardAction -> Cells -> (Cells, Int)
slideTo MoveLeft = slideWithTransform id id
slideTo MoveRight = slideWithTransform flipCells flipCells
slideTo MoveUp = slideWithTransform rotateCCW rotateCW
slideTo MoveDown = slideWithTransform rotateCW rotateCCW

slideWithTransform :: (Cells -> Cells) -> (Cells -> Cells) -> Cells -> (Cells, Int)
slideWithTransform transform transformBack cells = (transformBack slided, point)
  where (slided, point) = slideLeft $ transform cells

flipCells = map reverse
rotateCW = map reverse . transpose
rotateCCW = transpose . map reverse

slideLeft :: Cells -> (Cells, Int)
slideLeft cells = (map fst slides, sum $ map snd slides)
  where slides = map slide cells
        slide row = (merged ++ padding, point)
          where (merged, point) = mergePanels (filter (/= blankCell) row)
                padding = replicate (length row - length merged) blankCell

mergePanels :: [Int] -> ([Int], Int)
mergePanels ns = (map fst ps, sum $ map snd ps)
  where ps = mergePanelsWithPoint ns

mergePanelsWithPoint :: [Int] -> [(Int, Int)]
mergePanelsWithPoint [] = []
mergePanelsWithPoint [x] = [(x, 0)]
mergePanelsWithPoint (x:y:zs) = case merge x y of
                         Just m   -> (m, fib m) : mergePanelsWithPoint zs
                         Nothing  -> (x, 0) : mergePanelsWithPoint (y: zs)

merge :: Int -> Int -> Maybe Int
merge x y | x > y             = merge y x
          | x == 1 && y == 1  = Just 2
          | x + 1 == y        = Just $ y + 1
          | otherwise         = Nothing

fib = (fibonacci !!)
  where fibonacci = 1:1:zipWith (+) fibonacci (tail fibonacci)

replace i x ls = take i ls ++ [x] ++ drop (i + 1) ls
replace2 (i, j) x lss = replace i (replace j x (lss !! i)) lss
