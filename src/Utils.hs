module Utils where

import           Control.Monad
import           System.IO
import           Text.Printf
import           XMonad
import qualified XMonad.Actions.Warp   as Warp
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.StackSet       as W


type Key = (KeyMask, KeySym)


named name = Renamed.renamed [Renamed.Replace name]

padRight :: Int -> String -> String
padRight totalWidth string =
  string ++ replicate (totalWidth - length string) ' '

currentScreen :: X ScreenId
currentScreen = W.screen <$> W.current <$> gets windowset

currentWindow :: X (Maybe Window)
currentWindow =
  gets (W.peek . windowset)

currentWs :: X WorkspaceId
currentWs = W.currentTag <$> gets windowset

placeCursorMiddle :: X ()
placeCursorMiddle =
  currentScreen >>= \screen -> Warp.warpToScreen screen (1/2) (1/2)

logStr :: MonadIO m => String -> m ()
logStr = liftIO . hPutStrLn stderr . printf "[XMonad] %s"

logger :: (MonadIO m , Show s) => s -> m ()
logger = logStr . show

when_ :: MonadPlus m => Bool -> a -> m a
when_ bool m = if bool then return m else mzero
