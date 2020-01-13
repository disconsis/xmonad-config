{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- * Imports

import qualified Control.Foldl                       as Fold
import           Control.Monad
import           Control.Monad.Extra
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import           Numeric.Natural

import           Data.List
import           Data.Maybe
import qualified Data.Text                           as T
import Data.Char

import           System.Exit

import           Text.Printf                         (printf)
import Text.Read (readMaybe)

import           XMonad                              hiding ((|||))
import qualified XMonad.StackSet                     as W

import qualified XMonad.Prompt                       as Prompt
import qualified XMonad.Prompt.XMonad                as Prompt
import qualified XMonad.Prompt.Input                 as Prompt
import XMonad.Prompt.Input  ((?+))

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops           (ewmh)
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import           XMonad.Hooks.ManageHelpers          hiding (currentWs)

import qualified XMonad.Layout.Combo                 as Combo
import qualified XMonad.Layout.Decoration            as Decoration
import qualified XMonad.Layout.Gaps                  as Gaps
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.LimitWindows          as LimitWindows
import qualified XMonad.Layout.MultiToggle           as MultiToggle
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import qualified XMonad.Layout.NoBorders             as Borders
import qualified XMonad.Layout.PerWorkspace          as PerWorkspace
import qualified XMonad.Layout.Renamed               as Renamed
import qualified XMonad.Layout.Spacing               as Spacing
import qualified XMonad.Layout.TabBarDecoration      as TabDeco
import qualified XMonad.Layout.Tabbed                as Tabbed
import           XMonad.Layout.TwoPane               (TwoPane (..))
import qualified XMonad.Layout.WindowNavigation      as Nav

import qualified XMonad.Actions.CycleWS              as CycleWS
import qualified XMonad.Actions.GroupNavigation      as GroupNavigation
import qualified XMonad.Actions.SpawnOn              as SpawnOn
import qualified XMonad.Actions.Warp                 as Warp
import qualified XMonad.Actions.WorkspaceNames       as WorkspaceNames

import qualified XMonad.Util.Cursor                  as Cursor
import qualified XMonad.Util.ExtensibleState         as ExtState
import qualified XMonad.Util.EZConfig                as EZConfig
import qualified XMonad.Util.Paste                   as Paste
import qualified XMonad.Util.WorkspaceCompare        as WorkspaceCompare

import           Turtle                              hiding (printf)

import           Color
import           Polybar

-- * Features
-- ** Polybar
polybarPP :: X PP
polybarPP =
    namedGotoPP $
    onedarkPP

-- *** Theme
onedarkPP :: PP
onedarkPP = PP
    { ppCurrent          = Polybar.format [ Foreground onedarkBlack
                                          , Background onedarkGreen
                                          , Underline onedarkGreenDarker
                                          ] . pad
    , ppVisible          = Polybar.format [ Foreground onedarkGreen
                                          , Background grey
                                          , Underline onedarkGreen
                                          ] . pad
    , ppVisibleNoWindows = Just $ Polybar.format [ Foreground onedarkGrey
                                                 , Background grey
                                                 , Underline onedarkGrey
                                                 ] . pad
    , ppHidden           = Polybar.format [ Foreground onedarkGreen
                                          , Underline onedarkGreen
                                          ] . pad
    , ppHiddenNoWindows  = Polybar.format [ Foreground grey ] . pad
    , ppUrgent           = Polybar.format [ Foreground onedarkBlack
                                          , Background onedarkRed
                                          ] . pad
    , ppTitle            = Polybar.format [ Foreground onedarkGrey
                                          , Offset 20
                                          ] . shorten 50
    , ppLayout           = Polybar.format [ Foreground onedarkYellow ]
                           . padRight layoutDescriptionWidth
    , ppSep              = "  "
    , ppWsSep            = " "
    , ppTitleSanitize    = filter (`notElem` ['%', '{', '}'])
    , ppOrder            = layoutFirstOrder
    , ppSort             = WorkspaceCompare.getSortByIndex
    , ppExtras           = []
    , ppOutput           = const $ return ()
    }
    where
      layoutFirstOrder (workspaces : layout : title : extras) =
        layout : workspaces : title : extras
      layoutFirstOrder other = other


-- *** Workspace buttons, Named workspaces

namedGotoPP :: PP -> X PP
namedGotoPP pp = do
    names <- WorkspaceNames.getWorkspaceNames
    return $ pp
        { ppCurrent = ppCurrent pp . namedGoto names
        , ppVisible = ppVisible pp . namedGoto names
        , ppVisibleNoWindows =
            ppVisibleNoWindows pp >>= \f -> Just $ f . namedGoto names
        , ppHidden = ppHidden pp . namedGoto names
        , ppHiddenNoWindows = ppHiddenNoWindows pp . namedGoto names
        , ppUrgent = ppUrgent pp . namedGoto names
        }
      where
        namedGoto :: (String -> String) -> String -> String
        namedGoto rename ws = fromMaybe ws $ do
            wsNum <- elemIndex ws myWorkspaces
            let action = Action LeftClick $ printf "xdotool set_desktop %d" wsNum
            return $ Polybar.format [action] (rename ws)

-- * Workspaces

wsTodo  = "\xf00b"
wsConf  = "\xf992"
wsEntt  = "\xf880"
wsMusic = "\xf001"
wsComms = "\xf086"

myWorkspaces =
    [wsTodo] ++ fmap show [1..7] ++ [wsConf, wsEntt, wsMusic, wsComms]

-- * Prompt

blackWhitePrompt :: Prompt.XPConfig
blackWhitePrompt = def
    { -- Look
      Prompt.font = "xft:Iosevka Nerd Font:pixelsize=15"
    , Prompt.bgColor = white
    , Prompt.fgColor = black
      -- Location
    , Prompt.height = 26
    , Prompt.position = Prompt.CenteredAt { Prompt.xpCenterY = 0.988
                                          , Prompt.xpWidth = 0.15
                                          }
      -- Autocomplete
    , Prompt.historySize = 256
    , Prompt.historyFilter = Prompt.uniqSort
    , Prompt.showCompletionOnTab = False
    , Prompt.searchPredicate = \string completion -> isPrefixOf (toLower <$> string) (toLower <$> completion)
    , Prompt.autoComplete = Just 2000
    , Prompt.completionKey = (0, xK_Tab)
    , Prompt.maxComplRows = Just 5
    }

-- * Startup

myStartupHook :: X ()
myStartupHook = do
    EZConfig.checkKeymap myConfig myKeymap
    Cursor.setDefaultCursor Cursor.xC_left_ptr
    spawn "pgrep unclutter || unclutter --timeout 3"

-- * Layouts

myLayoutHook =
    ManageDocks.avoidStruts $
    Borders.smartBorders $
    myPerWorkspaceLayouts $
    MultiToggle.mkToggle (MultiToggle.single NBFULL) $
    myTall ||| myTabbed ||| myTwoPane


layoutDescriptionWidth =
    maximum . fmap length $
        [ description myTall
        , description myTabbed
        , description myTwoPane
        ]

-- ** Per workspace layouts

myPerWorkspaceLayouts = id
    . PerWorkspace.onWorkspace wsMusic myTabbed
    . PerWorkspace.onWorkspace wsComms myTabbed

-- ** Gaps

finalGap :: Num a => a
finalGap = 10

equalSpacing :: Integer -> Spacing.Border
equalSpacing gap =
    Spacing.Border
        { Spacing.top = gap
        , Spacing.bottom = gap
        , Spacing.left = gap
        , Spacing.right = gap
        }


mySpacing =
    Spacing.spacingRaw
        False                             -- ^ Smart Border
        (equalSpacing $ finalGap `div` 2) -- ^ Screen Border
        True                              -- ^ Screen Border enabled?
        (equalSpacing $ finalGap `div` 2) -- ^ Window Border
        True                              -- ^ Window Border enabled?

equalGaps :: Int -> [(Gaps.Direction2D, Int)]
equalGaps gap =
    flip (,) gap <$> [Gaps.U, Gaps.D, Gaps.L, Gaps.R]

myGaps =
    Gaps.gaps $ equalGaps finalGap

-- ** Tall

myTall :: Decoration.ModifiedLayout Renamed.Rename
          (Decoration.ModifiedLayout LimitWindows.LimitWindows
          (Decoration.ModifiedLayout Spacing.Spacing Tall))
          Window
myTall =
    named "Windowed" $
    LimitWindows.limitWindows 3 $
    mySpacing $
    Tall { tallNMaster        = 1
         , tallRatioIncrement = (3/100)
         , tallRatio          = (1/2)
         }

-- ** Tabbed

data EmptyShrinker = EmptyShrinker

instance Show EmptyShrinker where show _ = ""

instance Read EmptyShrinker where readsPrec _ s = [(EmptyShrinker, s)]

instance Tabbed.Shrinker EmptyShrinker where
    shrinkIt EmptyShrinker _ = []

myTabbed =
    named "Tabbed" $
    myGaps $
    Borders.noBorders $
    Tabbed.tabbedLeft EmptyShrinker onedarkTabbedTheme

onedarkTabbedTheme = def
    { Tabbed.activeColor         = activeColor
    , Tabbed.activeBorderColor   = activeColor
    , Tabbed.activeTextColor     = activeColor

    , Tabbed.inactiveColor       = inactiveColor
    , Tabbed.inactiveBorderColor = inactiveColor
    , Tabbed.inactiveTextColor   = inactiveColor

    , Tabbed.urgentColor         = urgentColor
    , Tabbed.urgentBorderColor   = urgentColor
    , Tabbed.urgentTextColor     = urgentColor

    , Tabbed.decoWidth           = 4
    , Tabbed.decoHeight          = 4
    }
    where
      activeColor = onedarkBlue
      inactiveColor = onedarkBlack
      urgentColor = onedarkRed

-- ** TwoPane

myTwoPane =
    named "TwoPane" $
    Borders.noBorders $
    Combo.combineTwo
        (TwoPane delta ratio)
        (leftGaps $ Tabbed.tabbedLeftAlways EmptyShrinker onedarkTabbedTheme)
        (rightGaps $ Tabbed.tabbedRightAlways EmptyShrinker onedarkTabbedTheme)
    where
      delta = (3/100)
      ratio = (1/2)
      leftGaps =
          Gaps.gaps [ (Gaps.U, finalGap)
                    , (Gaps.D, finalGap)
                    , (Gaps.L, finalGap)
                    , (Gaps.R, finalGap `div` 2)
                    ]
      rightGaps =
          Gaps.gaps [ (Gaps.U, finalGap)
                    , (Gaps.D, finalGap)
                    , (Gaps.L, finalGap `div` 2)
                    , (Gaps.R, finalGap)
                    ]

-- * ManageHook

myManageHook = composeAll
    [ myAppBorderSettings
    , myWindowShiftStaticSettings
    , SpawnOn.manageSpawn
    ]

-- ** App border settings

myAppBorderSettings = composeOne
    [ className =? "albert" -?> Borders.hasBorder False ]

-- ** Window shift settings (depending on static properties)

myWindowShiftStaticSettings = composeOne $
  [ -- only static properties here
  ]

-- * HandleEvent

myHandleEventHook =
    myWindowShiftDynamicSettings <+>
    Borders.borderEventHook

-- ** Window shift settings (depending on dynamic properties)
myWindowShiftDynamicSettings = dynamicPropertyChange "WM_CLASS" $ composeAll $
  [ className =? "Spotify" --> doShift wsMusic ]


-- * Logging

myLogHook :: X ()
myLogHook =
    GroupNavigation.historyHook

-- * Keys

myKeymap = concat
    [ myControlKeys
    , myMediaKeys
    , myMovementKeys
    , myResizeKeys
    , myLayoutKeys
    , myLauncherKeys
    , myInfoKeys
    , myWorkspaceNameKeys
    ]

myKeys config =
    EZConfig.mkKeymap config myKeymap

-- ** Control

cmdList :: [(String, X ())]
cmdList = [ ("set max volume", setMaxVolume) ]
  where
    setMaxVolume = do
      volString <- Prompt.inputPrompt blackWhitePrompt "Max Volume"
      fromMaybe mempty $ do
        maxVol <- volString >>= readMaybe
        return $ ExtState.put $ Volume maxVol

myControlKeys :: [(String, X ())]
myControlKeys =
    [ ("M-`", recompileXMonad)
    , ("M-S-C-`", io $ exitWith ExitSuccess)
    , ("M-S-<Space>", updateAllLayouts $ Layout myLayoutHook)
    , ("M-q", kill)
    , ("M-;", Prompt.xmonadPromptC cmdList blackWhitePrompt)
    ]
    where
      updateAllLayouts :: Layout Window -> X ()
      updateAllLayouts layout =
          sequence_ [updateLayout ws (Just layout) | ws <- myWorkspaces]

      recompileXMonad =
          spawn "xmonad --recompile && xmonad --restart;\
                \sleep 1; notify-send 'XMonad reloaded'"
          >> refresh

-- ** Media

newtype Volume = Volume Natural

instance ExtensionClass Volume where
  initialValue = Volume 150

myMediaKeys =
    [ -- volume
      ("<XF86AudioLowerVolume>", reduceVolume)
    , ("<XF86AudioRaiseVolume>", raiseVolume)
    , ("<XF86AudioMute>"       , toggleMute)
      -- player (e.g. Spotify)
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioPlay>", ensureSpotify >> spawn "playerctl play-pause")
      -- touchscreen
    , ("<XF86Search>", spawn "xinput-toggle -r elan -n")
      -- brightness
    , ("M-["  , setBrightness $ show brightnessStep ++ "%-")
    , ("M-]"  , setBrightness $ show brightnessStep ++ "%+")
    , ("M-S-[", setBrightness "1%")
    , ("M-S-]", setBrightness "100%")
      -- F11 without Fn key
    , ("<Print>", Paste.sendKey Paste.noModMask xK_F11)
     -- Wifi
    , ("<Home>", spawn "sudo connman_dmenu")
    ]
    where
      volumeStep = 5

      brightnessStep = 2

      raiseVolume = do
          (Volume maxVolume) <- ExtState.get
          spawn $ printf "[ $(pamixer --get-volume) -le %d ] \
                         \&& pamixer --allow-boost --increase %d"
              (maxVolume - volumeStep) volumeStep

      reduceVolume =
          spawn $ printf "pamixer --allow-boost --decrease %d" volumeStep

      toggleMute = spawn "pamixer --toggle-mute"

      setBrightness :: String -> X ()
      setBrightness val =
        spawn $ printf "sudo brightnessctl -d intel_backlight set %s" val


-- open spotify if no players running
ensureSpotify :: X ()
ensureSpotify =
  spawn "ensure-spotify.sh"
  -- numPlayers <- fold (inshell "playerctl --list-all" empty) Fold.length
  -- spawn $ printf "kitty --title=%d" numPlayers
  -- if numPlayers == 0 then
  --   SpawnOn.spawnOn wsMusic "spotify-pause-launch.sh"
  -- else
  --   spawn "kitty"

-- ** Movement

myMovementKeys =
    myWindowMovementKeys ++ myWorkspaceMovementKeys ++ myScreenMovementKeys


myWindowMovementKeys =
    [ -- moving focus
      ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-<Tab>", focusLastWindow)
      -- moving windows
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-m", windows W.swapMaster)
    , ("M-S-h", sendMessage $ Nav.Move Nav.L)
    , ("M-S-l", sendMessage $ Nav.Move Nav.R)
    ]
    where focusLastWindow =
              GroupNavigation.nextMatch GroupNavigation.History (return True)


myWorkspaceMovementKeys :: [(String, X ())]
myWorkspaceMovementKeys =
    [ (prefix ++ key, func ws)
      | (prefix, func) <- [ ("M-"  , viewWorkspace)
                          , ("M-S-", viewShift)
                          , ("M-C-", WorkspaceNames.swapWithCurrent)
                          ]
      , (key, ws) <- zip keys myWorkspaces
    ]
    where
      keys = fmap return $ ['0'..'9'] ++ ['-', '=']
      viewShift = windows . (liftM2 (.) W.greedyView W.shift)
      viewWorkspace ws =  windows (W.view ws) >> placeCursorMiddle


myScreenMovementKeys =
    [ ("M-y", switchScreen)  -- ^ switch focus to the next screen
    , ("M-S-y", swapScreens) -- ^ move currently focused window to next screen and switch focus to it
    ]
    where
      switchScreen = do
          CycleWS.nextScreen
          placeCursorMiddle
          -- move the cursor a little bit to get `unclutter` to show it
          spawn "xdotool mousemove_relative -- 1 0"
          spawn "xdotool mousemove_relative -- -1 0"

      swapScreens = do
          ws <- currentWs
          CycleWS.nextScreen
          windows $ W.greedyView ws

-- ** Resize

myResizeKeys =
    fmap sendMessage <$>
    [ ("M-h", Shrink)
    , ("M-l", Expand)
    ]

-- ** Layout

myLayoutKeys =
    [ -- switch layouts
      ("M-w", jumpToLayout myTall)
    , ("M-t", jumpToLayout myTabbed)
      -- , ("M-p", jumpToLayout myTwoPane)
      -- modify spacing, full etc
    , ("M-f", sendMessage (MultiToggle.Toggle NBFULL)
        >> sendMessage ManageDocks.ToggleStruts)
    , ("M-S-f", sendMessage ManageDocks.ToggleStruts)
    ]
    where
      jumpToLayout :: LayoutClass layout a => layout a -> X ()
      jumpToLayout = sendMessage . JumpToLayout . description


-- ** Launcher

myLauncherKeys =
    fmap spawn <$>
    [ ("M-u t",   terminal myConfig)
    , ("M-u g",   "google-chrome-stable")
    , ("M-u r",   "$TERMINAL ranger")
    , ("M-u e",   "emacsclientserver.sh")
    , ("M-u S-e", "emacs --debug-init")
    , ("M-u q",   "qutebrowser")
    , ("M-u w",   "whatsapp.sh")
    , ("M-u m",   "gmail.sh")
    ]

-- ** Info

myInfoKeys =
    fmap spawn <$>
    [ ("M-i n", "nerdfont-dmenu.sh")
    , ("M-i l", "google-chrome-app http://detexify.kirelabs.org/classify.html")
    , ("M-i u", copyUniversityId)
    , ("M-i i", "xdg-open ~/refer/iris-documentation.pdf")
    ]
    where
      copyUniversityId =
          "echo 'N11092138' | xclip -selection clipboard;\
          \notify-send 'University Info' 'Copied University ID to clipboard'"

-- ** Naming workspaces

myWorkspaceNameKeys =
    [ ("M-r", WorkspaceNames.renameWorkspace blackWhitePrompt)
    , ("M-S-r", WorkspaceNames.setCurrentWorkspaceName mempty)
    , ("M-C-r", forM_ myWorkspaces (flip WorkspaceNames.setWorkspaceName mempty))
    ]

-- * Utils

named name = Renamed.renamed [Renamed.Replace name]

padRight totalWidth string =
  string ++ replicate (totalWidth - length string) ' '

currentScreen :: X ScreenId
currentScreen = W.screen <$> W.current <$> gets windowset

currentWs :: X WorkspaceId
currentWs = W.currentTag <$> gets windowset

placeCursorMiddle :: X ()
placeCursorMiddle =
  currentScreen >>= \screen -> Warp.warpToScreen screen (1/2) (1/2)

-- * Main

myConfig = def
    { terminal           = "kitty tmux -2"
    , focusedBorderColor = onedarkBlue
    , normalBorderColor  = onedarkBlack
    , borderWidth        = 2
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , keys               = myKeys
    , workspaces         = myWorkspaces
    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , logHook            = myLogHook
    }

main :: IO ()
main = do
    config <-
        Polybar.manage polybarPP $
        ewmh $
        myConfig
    xmonad config
