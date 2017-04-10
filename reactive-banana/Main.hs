{-# LANGUAGE RecursiveDo #-}

import Prelude hiding (lookup)
import Control.Monad.Fix
import Data.List (init, transpose)
import Data.Map.Strict (Map(..), fromList, lookup)
import Gloss.FRP.Reactive.Banana (playReactive, InputEvent)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Reactive.Banana.Combinators
import qualified Reactive.Banana.Combinators as FRP
import Reactive.Banana.Frameworks
import qualified System.Random.MWC as Random

-- ゲーム上の位置を表す
type Position = (Int, Int)

-- Board
type Cells = [[Int]]
data Board = Board
  { _cells   :: Cells
  }

-- Board actions
data BoardAction = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show

-- ゲームの状態は蛇とターゲットの組で表される
data GameState = GameState
  { _board  :: Board
  }

-- シーンを表す
data GameScene = MainGame deriving Show

-- Image resource manager
type ImageResourceManager = Map String Picture

getImg :: ImageResourceManager -> String -> Maybe Picture
getImg = flip lookup

-- シーンの実装の型
type GameSceneHandler =  ImageResourceManager
                      -> Random.GenIO                -- 乱数のシード
                      -> Handler GameScene           -- シーンを遷移するための関数（一回しか呼んではならない。乱用禁止！）
                      -> FRP.Event Float             -- 1フレームごとに発火するイベント
                      -> FRP.Event InputEvent        -- 外部入力ごとに発火するイベント
                      -> MomentIO (Behavior Picture) -- 実行結果として描画する画面

windowWidth = 640 :: Int
windowHeight = 960 :: Int

blockSize = 100

areaWidth = windowWidth `div` (round blockSize * 2) - 1
areaHeight = windowHeight `div` (round blockSize * 2) - 1

makeColor8 r g b a = makeColor (r / 255.0) (g / 255.0) (b / 255.0) (a / 255.0)
bgColor = makeColor8 0xbb 0xad 0x9f 0xff
tileColor = makeColor8 0xee 0xe4 0xd9 0xff

replace ls i x = take i ls ++ [x] ++ drop (i + 1) ls
replace2 lss (i, j) x = replace lss i $ replace (lss !! i) j x

-- Replace one cell on a board
replaceBoard :: Board -> (Int, Int) -> Int -> Board
replaceBoard board pos x = Board $ replace2 (_cells board) pos x

-- Initial board
initialBoard :: Board
initialBoard = replaceBoard (replaceBoard (replaceBoard emptyBoard (0, 0) 1) (1, 2) 2) (0, 3) 1
  where emptyBoard = Board (replicate 4 $ replicate 4 0)

-- Convert event to (maybe) action
event2Action :: InputEvent -> Maybe BoardAction
event2Action (EventKey (SpecialKey KeyUp)    Down _ _) = Just MoveUp
event2Action (EventKey (SpecialKey KeyDown)  Down _ _) = Just MoveDown
event2Action (EventKey (SpecialKey KeyLeft)  Down _ _) = Just MoveLeft
event2Action (EventKey (SpecialKey KeyRight) Down _ _) = Just MoveRight
event2Action _                                         = Nothing

updateBoardCells :: (Cells -> Cells) -> Board -> Board
updateBoardCells f board = Board $ f (_cells board)

updateBoard :: BoardAction -> Board -> Board
updateBoard MoveLeft = updateBoardCells updateBoardLeft
updateBoard MoveRight = updateBoardCells updateBoardRight
updateBoard MoveUp = updateBoardCells updateBoardUp
updateBoard MoveDown = updateBoardCells updateBoardDown

updateBoardLeft, updateBoardRight, updateBoardUp, updateBoardDown :: Cells -> Cells
updateBoardLeft = map (\row -> take 4 $ mergePanels (filter (> 0) row) ++ repeat 0)
updateBoardRight = map reverse . updateBoardLeft . map reverse
updateBoardUp = transpose . updateBoardLeft . transpose
updateBoardDown = transpose . updateBoardRight . transpose

mergePanels :: [Int] -> [Int]
mergePanels [] = []
mergePanels [x] = [x]
mergePanels (x:y:zs) = case merge x y of
                         Just m   -> m : mergePanels zs
                         Nothing  -> x : mergePanels (y: zs)

merge :: Int -> Int -> Maybe Int
merge x y | x > y             = merge y x
          | x == 1 && y == 1  = Just 2
          | x + 1 == y        = Just $ y + 1
          | otherwise         = Nothing

-- ゲーム上の一マスを描画する関数
tile :: Color -> Position -> Picture
tile c (x, y) =
  let (x', y') = (fromIntegral x * blockSize, fromIntegral y * blockSize)
   in translate x' y' $ color c $ rectangleSolid blockSize blockSize

-- Draw board
drawBoard :: ImageResourceManager -> Board -> Picture
drawBoard imgResMgr board = pictures $ concat $ zipWith drawRow (_cells board) [0..]
  where drawRow row iy = zipWith drawCell row [0..]
          where drawCell c ix = translate x' y' $ color tileColor $ ((getImg imgResMgr $ show c) `orJust` blank)
                  where (x', y') = (fromIntegral ix * blockSize - blockSize * 1.5, blockSize * 1.5 - fromIntegral iy * blockSize)
                        boxSize = 140
                        margin = 8
                        blockSize = 120  --boxSize + margin * 2
                        size = 4

orJust :: Maybe a -> a -> a
orJust (Just x) _ = x
orJust Nothing  y = y

-- ゲームの状態を描画する関数
drawGameState :: ImageResourceManager -> GameState -> Picture
drawGameState imgResMgr gs = pictures [drawBoard imgResMgr (_board gs)]

-- ゲームのシーンからその処理関数を取得する
getHandler :: GameScene -> GameSceneHandler
getHandler MainGame   = bMainGame

-- ゲーム画面
bMainGame :: GameSceneHandler
bMainGame imgResMgr gen sceneHandler eTick eEvent = do
  -- ゲームを実行する間隔を制御するための Event
  let eGameStep = eTick

  -- Keyboard event behavior
  let eActionEvent = filterJust $ fmap event2Action eEvent

  -- 蛇とターゲットのBehavior
  (bBoard) <- mdo
    bBoard <- accumB initialBoard (updateBoard <$> eActionEvent)

    pure (bBoard)

  pure $ fmap (drawGameState imgResMgr) (GameState <$> bBoard)

imageResources = [ "0"
                 , "1"
                 , "2"
                 , "3"
                 , "4"
                 ]

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
