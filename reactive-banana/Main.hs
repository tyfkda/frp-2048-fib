{-# LANGUAGE RecursiveDo #-}

import Prelude hiding (lookup)
import Control.Monad.Fix
import Data.List (transpose)
import Data.Map.Strict (Map(..), fromList, lookup)
import Data.Maybe (fromMaybe)
import Gloss.FRP.Reactive.Banana (playReactive, InputEvent)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Reactive.Banana.Combinators
import qualified Reactive.Banana.Combinators as FRP
import Reactive.Banana.Frameworks
import qualified System.Random.MWC as Random

-- Board
type Cells = [[Int]]
data Board = Board
  { _cells   :: Cells
  }

blankCell = 0

-- Board actions
data BoardAction = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show

-- Game state
data GameState = GameState
  { _board  :: Board
  , _score :: Int
  }

-- Represent scene
data GameScene = MainGame deriving Show

-- Image resource manager
type ImageResourceManager = Map String Picture

getImg :: ImageResourceManager -> String -> Picture
getImg imgResMgr name = fromMaybe blank $ lookup name imgResMgr

-- Type to implement scene
type GameSceneHandler =  ImageResourceManager
                      -> Random.GenIO                -- Random seed
                      -> Handler GameScene           -- Scene transition function (call just once, do not abuse!)
                      -> FRP.Event Float             -- Event which is triggered every frame
                      -> FRP.Event InputEvent        -- Event which is triggered by input
                      -> MomentIO (Behavior Picture) -- Result picture of execution

windowWidth = 640 :: Int
windowHeight = 960 :: Int

makeColor8 r g b a = makeColor (r / 255.0) (g / 255.0) (b / 255.0) (a / 255.0)
bgColor = makeColor8 0xbb 0xad 0x9f 0xff
tileColor = makeColor8 0xee 0xe4 0xd9 0xff

replace ls i x = take i ls ++ [x] ++ drop (i + 1) ls
replace2 lss (i, j) x = replace lss i $ replace (lss !! i) j x

-- Replace one cell on a board
replaceBoard :: (Int, Int) -> Int -> Board -> Board
replaceBoard pos x board = Board $ replace2 (_cells board) pos x

-- Initial board
initialBoard :: Int -> Board
initialBoard size = replaceBoard (0, 3) 1 $ replaceBoard (1, 2) 2 $ replaceBoard (0, 0) 1 emptyBoard
  where emptyBoard = Board (replicate size $ replicate size blankCell)

boardSize :: Board -> Int
boardSize = length . _cells

-- Convert event to (maybe) action
event2Action :: InputEvent -> Maybe BoardAction
event2Action (EventKey (SpecialKey KeyUp)    Down _ _) = Just MoveUp
event2Action (EventKey (SpecialKey KeyDown)  Down _ _) = Just MoveDown
event2Action (EventKey (SpecialKey KeyLeft)  Down _ _) = Just MoveLeft
event2Action (EventKey (SpecialKey KeyRight) Down _ _) = Just MoveRight
event2Action _                                         = Nothing

updateBoardCells :: (Cells -> Cells) -> Board -> Board
updateBoardCells f board = Board $ f (_cells board)

updateBoard :: Int -> BoardAction -> Board -> Board
updateBoard r act board
  | cells == slided  = board  -- Cannot move to the direction
  | otherwise        = replaceBoard (0, 0) r $ Board slided
  where cells = _cells board
        (slided, point) = slideTo act cells

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

-- Draw board
drawBoard :: ImageResourceManager -> Board -> Picture
drawBoard imgResMgr board = pictures $ concat $ zipWith drawRow (_cells board) [0..]
  where drawRow row iy = zipWith drawCell row [0..]
          where drawCell c ix = translate x' y' $ color tileColor $ getImg imgResMgr $ show c
                  where (x', y') = (fromIntegral ix * blockSize - blockSize * 1.5, blockSize * 1.5 - fromIntegral iy * blockSize)
                        blockSize = 120

-- Draw score
drawScore :: Int -> Picture
drawScore score = translate (negate 200) 300 $ scale 0.25 0.25 $ color black $ text $ "Score: " ++ show score

-- Draw function for game state
drawGameState :: ImageResourceManager -> GameState -> Picture
drawGameState imgResMgr gs = pictures [drawBoard imgResMgr (_board gs), drawScore (_score gs)]

-- Return handler for scene
getHandler :: GameScene -> GameSceneHandler
getHandler MainGame   = bMainGame


genRandomPosition :: MonadIO io => Random.GenIO -> io Int
genRandomPosition gen = do
  --r <- liftIO $ Random.uniformR (0, product [1..10]) gen
  r <- liftIO $ Random.uniformR (1, 1) gen
  pure r


-- Game screen
bMainGame :: GameSceneHandler
bMainGame imgResMgr gen sceneHandler eTick eEvent = do
  -- Event for game interval
  let eGameStep = eTick

  -- Keyboard event behavior
  let eActionEvent = filterJust $ fmap event2Action eEvent

  -- Behavior
  (bBoard, bScore) <- mdo
    let eMove = eActionEvent
    eRnd <- execute $ genRandomPosition gen <$ eGameStep
    bRnd <- stepper 0 eRnd
    bBoard <- accumB (initialBoard 4) (updateBoard <$> bRnd <@> eMove)
    bScore <- accumB 0 ((+ 1) <$ eMove)

    pure (bBoard, bScore)

  pure $ fmap (drawGameState imgResMgr) (GameState <$> bBoard <*> bScore)

imageResources = map show [0..19]

loadImageResources :: [String] -> IO ImageResourceManager
loadImageResources fns = do
  imgs <- mapM (loadBMP . (\fn -> "data/" ++ fn ++ ".bmp")) fns
  return $ fromList $ zip fns imgs

main :: IO ()
main = do
  gen <- Random.createSystemRandom
  let window = InWindow "2048 Fibonacci" (windowWidth, windowHeight) (100, 100)
  imgResMgr <- loadImageResources imageResources
  playReactive window bgColor 60 $ \eTick eEvent -> do
    (eScene, sceneHandler) <- newEvent
    bScene <- execute $ fmap (\s -> getHandler s imgResMgr gen sceneHandler eTick eEvent) eScene
    initial <- bMainGame imgResMgr gen sceneHandler eTick eEvent
    switchB initial bScene
