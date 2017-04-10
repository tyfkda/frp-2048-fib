{-# LANGUAGE RecursiveDo #-}

import Control.Monad.Fix
import Data.List
import Gloss.FRP.Reactive.Banana (playReactive, InputEvent)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Reactive.Banana.Combinators
import qualified Reactive.Banana.Combinators as FRP
import Reactive.Banana.Frameworks
import qualified System.Random.MWC as Random

-- ゲーム上の位置を表す
type Position = (Int, Int)

-- 蛇は自身の長さと全ての節のゲーム上の位置を持つ
data Snake = Snake
  { _body   :: Position
  }

-- 蛇が取れる行動
data SnakeAction = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show

-- ゲームの状態は蛇とターゲットの組で表される
data GameState = GameState
  { _snake  :: Snake
  }

-- シーンを表す
data GameScene = MainGame deriving Show

-- シーンの実装の型
type GameSceneHandler =  Random.GenIO                -- 乱数のシード
                      -> Handler GameScene           -- シーンを遷移するための関数（一回しか呼んではならない。乱用禁止！）
                      -> FRP.Event Float             -- 1フレームごとに発火するイベント
                      -> FRP.Event InputEvent        -- 外部入力ごとに発火するイベント
                      -> MomentIO (Behavior Picture) -- 実行結果として描画する画面

windowWidth = 640 :: Int
windowHeight = 480 :: Int

blockSize = 20

areaWidth = windowWidth `div` (round blockSize * 2) - 1
areaHeight = windowHeight `div` (round blockSize * 2) - 1

-- 蛇の初期状態
initialSnake :: Snake
initialSnake = Snake (-10, -10)

-- ゲーム上の一マスを描画する関数
tile :: Color -> Position -> Picture
tile c (x, y) =
  let (x', y') = (fromIntegral x * blockSize, fromIntegral y * blockSize)
   in translate x' y' $ color c $ rectangleSolid blockSize blockSize

-- 蛇を描画する関数
drawSnake :: Snake -> Picture
drawSnake snake = tile white $ _body snake

-- ゲームの状態を描画する関数
drawGameState :: GameState -> Picture
drawGameState gs = pictures [drawSnake (_snake gs)]

-- ゲームのシーンからその処理関数を取得する
getHandler :: GameScene -> GameSceneHandler
getHandler MainGame   = bMainGame

-- ゲーム画面
bMainGame :: GameSceneHandler
bMainGame gen sceneHandler eTick eEvent = do
  -- ゲームを実行する間隔を制御するための Event
  let eGameStep = eTick

  -- 蛇とターゲットのBehavior
  (bSnake) <- mdo
    bSnake <- accumB initialSnake (pure id <@ eGameStep)

    pure (bSnake)

  pure $ fmap drawGameState (GameState <$> bSnake)

main :: IO ()
main = do
  gen <- Random.createSystemRandom
  let window = InWindow "Snake Game" (windowWidth, windowHeight) (100, 100)
  playReactive window black 60 $ \eTick eEvent -> do
    (eScene, sceneHandler) <- newEvent
    bScene <- execute $ fmap (\s -> getHandler s gen sceneHandler eTick eEvent) eScene
    initial <- bMainGame gen sceneHandler eTick eEvent
    switchB initial bScene
