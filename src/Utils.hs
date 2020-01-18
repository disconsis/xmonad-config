module Utils where


import           XMonad
import qualified XMonad.Actions.Warp   as Warp
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.StackSet       as W

import           System.IO

import           Text.Printf


named name = Renamed.renamed [Renamed.Replace name]

padRight :: Int -> String -> String
padRight totalWidth string =
  string ++ replicate (totalWidth - length string) ' '

currentScreen :: X ScreenId
currentScreen = W.screen <$> W.current <$> gets windowset

currentWs :: X WorkspaceId
currentWs = W.currentTag <$> gets windowset

placeCursorMiddle :: X ()
placeCursorMiddle =
  currentScreen >>= \screen -> Warp.warpToScreen screen (1/2) (1/2)

logMsg :: String -> X ()
logMsg = liftIO . hPutStrLn stderr . printf "[XMonad] %s"
