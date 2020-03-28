{-# LANGUAGE FlexibleContexts #-}

module Polybar ( MouseButton(..)
               , Formatting(..)
               , formatOne
               , format
               , Polybar.manage
               , screenNames
               , polybarStartup
               , polybarCleanup
               , polybarLog
               ) where

import           Control.Monad
import           Control.Monad.Extra
import           Data.Maybe

import           Text.Printf
import           Text.Read

import           XMonad
import           XMonad.Config.Dmwit          (outputOf, splitColon)
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.ManageDocks     as ManageDocks
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Util.Run

import           Utils

import           System.IO
import           System.Posix.Files

-- * Formatting

-- | Mouse buttons for polybar.
data MouseButton
  = LeftClick
  | MiddleClick
  | RightClick
  | ScrollUp
  | ScrollDown
  | DoubleLeftClick
  | DoubleMiddleClick
  | DoubleRightClick
  deriving (Eq, Ord, Show, Enum)


-- | Formatting options for polybar.
data Formatting
  = Foreground String
  | Background String
  | Reverse
  | Underline String
  | Overline String
  | Font Int
  | Offset Int
  | Action MouseButton String
  | AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Eq, Show)


bracket :: String -> String
bracket =
    wrap "%{" "}"


-- | Apply a single formatting option.
formatOne :: Formatting -> String -> String
formatOne (Foreground color) = wrap (bracket $ "F" ++ color) (bracket "F-")
formatOne (Background color) = wrap (bracket $ "B" ++ color) (bracket "B-")
formatOne Reverse            = wrap (bracket "R") (bracket "R")
formatOne (Underline color)  = wrap (bracket ("U" ++ color) ++ bracket "+u") (bracket "-u")
formatOne (Overline color)   = wrap (bracket ("O" ++ color) ++ bracket "+o") (bracket "-o")
formatOne (Font index)       = wrap (bracket "T" ++ show index) (bracket "T-")
formatOne (Offset size)      = (bracket ("O" ++ show size) ++)
formatOne (Action button command) =
  wrap (bracket (printf "A%d:%s:" (fromEnum button + 1) command)) (bracket "A")
formatOne AlignLeft          = (bracket "l" ++)
formatOne AlignCenter        = (bracket "c" ++)
formatOne AlignRight         = (bracket "r" ++)


-- | Implementation of [[polybar formatting rules][https://github.com/jaagr/polybar/wiki/Formatting]].
format :: [Formatting] -> String -> String
format fmt = foldr (.) id (fmap formatOne fmt)

-- * Setup & Multiple bars

screenNames :: IO [(ScreenId, String)]
screenNames = do
  output <-
    outputOf "xrandr --listactivemonitors | awk '{print $1 $4}'"
  return $ mapMaybe parseItem $ drop 1 $ lines output

  where
    parseItem :: String -> Maybe (ScreenId, String)
    parseItem s = case splitColon s of
      [a,b] ->
        case readMaybe a of
          Just a' -> Just (S a', b)
          Nothing -> Nothing

      _ -> Nothing

polybarStartup :: ScreenId -> IO Handle
polybarStartup screenId = do
  spawn "polybar-launch"
  screenName <- fromJust <$> lookup screenId <$> screenNames
  logStr screenName -- This is required (maybe to force strictness?)
  let fonts =
        [ "ypn envypn:size=13"
        , "Iosevka Nerd Font:size=10"
        , "Material Icons:size=11"
        , "FuraCode Nerd Font:size=10"
        , "siji:size=10"
        , "file-icons:size=10"
        ]
  let fontString = concatMap (wrap "-f \"" "\" ") $ take 10 fonts
  let lemonbarCmd = printf "lemonbar -p -b -o -2 %s -O %s | bash" fontString screenName
  logStr lemonbarCmd
  spawnPipe lemonbarCmd

polybarCleanup :: IO ()
polybarCleanup = do
  spawn "pkill -9 lemonbar"
  spawn "pkill polybar"

polybarLog :: X PP -> X PP -> X ()
polybarLog focusedPP unfocusedPP = do
  focused' <- focusedPP
  unfocused' <- unfocusedPP
  multiPP focused' unfocused'

-- | Modify a config to handle polybar.
manage :: LayoutClass l Window
       => X PP
       -> X PP
       -> XConfig l
       -> XConfig (ModifiedLayout ManageDocks.AvoidStruts l)
manage focusedPP unfocusedPP config =
  ManageDocks.docks $ config
    { startupHook = startupHook config <+>
        dynStatusBarStartup polybarStartup polybarCleanup
    , handleEventHook = handleEventHook config <+>
        dynStatusBarEventHook polybarStartup polybarCleanup <+>
        ManageDocks.docksEventHook
    , logHook = logHook config <+>
        polybarLog focusedPP unfocusedPP
    , layoutHook = ManageDocks.avoidStruts $ layoutHook config
    , manageHook = manageHook config <+> ManageDocks.manageDocks
    }
