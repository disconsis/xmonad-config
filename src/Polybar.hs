{-# LANGUAGE FlexibleContexts #-}

module Polybar ( MouseButton(..)
               , Formatting(..)
               , formatOne
               , format
               , Polybar.manage
               ) where

import qualified Codec.Binary.UTF8.String     as UTF8
import qualified DBus
import qualified DBus.Client                  as DBus
import           Text.Printf

import           XMonad
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.ManageDocks     as ManageDocks
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Util.SpawnOnce        (spawnOnce)


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


-- | Setup a dbus client for polybar.
init :: IO DBus.Client
init = do
  dbus <- DBus.connectSession
  DBus.requestName dbus (DBus.busName_ "org.xmonad.log")
    [ DBus.nameAllowReplacement
    , DBus.nameReplaceExisting
    , DBus.nameDoNotQueue
    ]
  return dbus


-- | Emit a DBus signal on log updates.
dbusOutput :: DBus.Client -> String -> IO ()
dbusOutput client str = do
    DBus.emit client signal
  where
    signal = (DBus.signal objectPath interfaceName memberName)
             { DBus.signalBody = [DBus.toVariant $ UTF8.decodeString str] }
    objectPath = DBus.objectPath_ "/org/xmonad/Log"
    interfaceName = DBus.interfaceName_ "org.xmonad.Log"
    memberName = DBus.memberName_ "Update"


-- | Set the pretty printer to output to dbus.
dbusPP :: DBus.Client -> PP -> PP
dbusPP client pp = pp { ppOutput = ppOutput pp >> dbusOutput client }


-- | Modify a config to handle polybar.
manage :: LayoutClass l Window
       => X PP
       -> XConfig l
       -> IO (XConfig (ModifiedLayout ManageDocks.AvoidStruts l))
manage pp config = do
    client <- Polybar.init
    return $ ManageDocks.docks $ config
      { startupHook = spawnOnce "~/.config/polybar/launch.sh" >> startupHook config
      , layoutHook = ManageDocks.avoidStruts $ layoutHook config
      , logHook = (dynamicLogWithPP =<< (dbusPP client) <$> pp) <+> logHook config
      }
