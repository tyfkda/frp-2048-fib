{-# LANGUAGE RecursiveDo #-}

import Control.Monad.Fix
import Gloss.FRP.Reactive.Banana (playReactive, InputEvent)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Reactive.Banana.Combinators
import qualified Reactive.Banana.Combinators as FRP
import Reactive.Banana.Frameworks
import qualified System.Random.MWC as Random

import Board ( Board(..), BoardAction(..)
             , canMove, initialBoard, updateBoard )
import ImageResourceManager ( ImageResourceManager
                            , getImg, loadImageResources )

-- Utility function for reactive-banana
-- Pass first `n` Events, and drop after ones.
takeE :: Int -> FRP.Event a -> MomentIO (FRP.Event a)
takeE n event = do
  bCount <- accumB 0 ((+1) <$ event)
  pure $ fmap snd $ filterE (\(c,_) -> c < n) ((,) <$> bCount <@> event)

-- Normal key pressed?
isKeyPressed :: Char -> InputEvent -> Bool
isKeyPressed k (EventKey (Char c)       _ _ _) = k == c
isKeyPressed _ _                               = False

-- Game state
data GameState = GameState
  { _board  :: Board
  }

-- Represent scene
data GameScene = MainGame | GameOverScene deriving Show

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

-- Convert event to (maybe) action
event2BoardAction :: InputEvent -> Maybe BoardAction
event2BoardAction (EventKey (SpecialKey KeyUp)    Down _ _) = Just MoveUp
event2BoardAction (EventKey (SpecialKey KeyDown)  Down _ _) = Just MoveDown
event2BoardAction (EventKey (SpecialKey KeyLeft)  Down _ _) = Just MoveLeft
event2BoardAction (EventKey (SpecialKey KeyRight) Down _ _) = Just MoveRight
event2BoardAction _                                         = Nothing

-- Draw board
drawBoard :: ImageResourceManager -> Board -> Picture
drawBoard imgResMgr board = pictures $ concat $ zipWith drawRow (_cells board) [0..]
  where drawRow row iy = zipWith drawCell row [0..]
          where drawCell c ix = translate x' y' $ color tileColor $ getImg imgResMgr $ show c
                  where (x', y') = (fromIntegral ix * blockSize - blockSize * 1.5, blockSize * 1.5 - fromIntegral iy * blockSize)
                        blockSize = 120

-- Draw score
drawScore :: Int -> Picture
drawScore score = translate (-200) 300 $ scale 0.25 0.25 $ color black $ text $ "Score: " ++ show score

-- Draw function for game state
drawGameState :: ImageResourceManager -> GameState -> Picture
drawGameState imgResMgr gs = pictures [drawBoard imgResMgr (_board gs), drawScore (_score (_board gs))]

-- Return handler for scene
getHandler :: GameScene -> GameSceneHandler
getHandler MainGame   = bMainGame
getHandler GameOverScene = bGameOver


genRandom :: MonadIO io => (Int, Int) -> Random.GenIO -> io Int
genRandom range gen = do
  r <- liftIO $ Random.uniformR range gen
  pure r

-- Game screen
bMainGame :: GameSceneHandler
bMainGame imgResMgr gen sceneHandler eTick eEvent = do
  -- Event for game interval
  let eGameStep = eTick

  -- Keyboard event behavior
  let eActionEvent = filterJust $ fmap event2BoardAction eEvent

  -- Behavior
  bBoard <- mdo
    let eMove = eActionEvent
    eRnd <- execute $ genRandom (0, product [1..4] * product [1..4] * product [1..6]) gen <$ eGameStep
    bRnd <- stepper 0 eRnd
    bBoard <- accumB (initialBoard 4) (updateBoard <$> bRnd <@> eMove)

    pure bBoard

  eGameOver <- takeE 1 $ filterE id $ fmap (not . canMove) bBoard <@ eActionEvent
  reactimate $ (liftIO $ sceneHandler GameOverScene) <$ eGameOver

  pure $ fmap (drawGameState imgResMgr) (GameState <$> bBoard)

-- Game over screen
bGameOver :: GameSceneHandler
bGameOver imgResMgr gen sceneHandler eTick eEvent = do
  let title       = translate (-200) 0     $ scale 0.5  0.5  $ color red   $ text "Game over"
      description = translate (-200) (-50) $ scale 0.25 0.25 $ color white $ text "Press 'r' key to restart."

  eAnyKeyPressed <- takeE 1 $ pure (isKeyPressed 'r') `filterApply` eEvent
  reactimate $ (liftIO $ sceneHandler MainGame) <$ eAnyKeyPressed

  pure $ (pure (pictures [title, description]))

imageResourceNames = map show [0..19]

main :: IO ()
main = do
  gen <- Random.createSystemRandom
  let window = InWindow "2048 Fibonacci" (windowWidth, windowHeight) (100, 100)
  imgResMgr <- loadImageResources imageResourceNames
  playReactive window bgColor 60 $ \eTick eEvent -> do
    (eScene, sceneHandler) <- newEvent
    bScene <- execute $ fmap (\s -> getHandler s imgResMgr gen sceneHandler eTick eEvent) eScene
    initial <- bMainGame imgResMgr gen sceneHandler eTick eEvent
    switchB initial bScene
