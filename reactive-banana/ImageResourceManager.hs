module ImageResourceManager
    ( ImageResourceManager
    , getImg, loadImageResources
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map(..), fromList, lookup)
import Data.Maybe (fromMaybe)
import Graphics.Gloss

-- Image resource manager
type ImageResourceManager = Map String Picture

loadImageResources :: [String] -> IO ImageResourceManager
loadImageResources fns = do
  imgs <- mapM (loadBMP . (\fn -> "data/" ++ fn ++ ".bmp")) fns
  return $ fromList $ zip fns imgs

getImg :: ImageResourceManager -> String -> Picture
getImg imgResMgr name = fromMaybe blank $ lookup name imgResMgr
