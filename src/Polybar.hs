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
  deriving (Eq, Show)


bracket :: String -> String
bracket =
    wrap "%{" "}"


-- | Apply a single formatting option.
formatOne :: Formatting -> String -> String
formatOne (Foreground color) = wrap (bracket $ "F" ++ color) (bracket "F-")
formatOne (Background color) = wrap (bracket $ "B" ++ color) (bracket "B-")
formatOne Reverse            = wrap (bracket "R") (bracket "R")
formatOne (Underline color)  = wrap (bracket ("u" ++ color) ++ bracket "+u") (bracket "-u")
formatOne (Overline color)   = wrap (bracket ("o" ++ color) ++ bracket "+o") (bracket "-o")
formatOne (Font index)       = wrap (bracket "T" ++ show index) (bracket "T-")
formatOne (Offset size)      = (bracket ("O" ++ show size) ++)
formatOne (Action button command) =
  wrap (bracket (printf "A%d:%s:" (fromEnum button + 1) command)) (bracket "A")


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
  screenName <- fromJust <$> lookup screenId <$> screenNames
  spawnPipe $ "polybar-start-monitor " ++ screenName

polybarCleanup :: IO ()
polybarCleanup = do
  spawn "pkill -9 polybar"

-- TODO: This takes a lot of time to update sometimes
polybarLog :: X PP -> X PP -> X ()
polybarLog focusedPP unfocusedPP =
  join $ multiPP <$> focusedPP <*> unfocusedPP

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
        dynStatusBarEventHook polybarStartup polybarCleanup
    , logHook = logHook config <+>
        polybarLog focusedPP unfocusedPP
    , layoutHook = ManageDocks.avoidStruts $ layoutHook config
    }
