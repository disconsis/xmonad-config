{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- * Imports

import           Control.Monad

import           Numeric.Natural

import           Data.Char
import           Data.List
import qualified Data.Map                            as M
import           Data.Maybe

import           System.Exit

import           Text.Printf                         (printf)
import           Text.Read                           (readMaybe)

import           XMonad                              hiding ((|||))
import qualified XMonad.StackSet                     as W

import qualified XMonad.Prompt                       as Prompt
import qualified XMonad.Prompt.Input                 as Prompt
import qualified XMonad.Prompt.XMonad                as Prompt

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops           (ewmh)
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import           XMonad.Hooks.ManageHelpers          hiding (currentWs)
import           XMonad.Hooks.SetWMName              (setWMName)

import qualified XMonad.Layout.Decoration            as Decoration
import           XMonad.Layout.Dwindle               as Dwindle
import qualified XMonad.Layout.Gaps                  as Gaps
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.LimitWindows          as LimitWindows
import qualified XMonad.Layout.MultiToggle           as MultiToggle
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import qualified XMonad.Layout.NoBorders             as Borders
import           XMonad.Layout.OneBig
import qualified XMonad.Layout.PerWorkspace          as PerWorkspace
import qualified XMonad.Layout.Renamed               as Renamed
import qualified XMonad.Layout.Spacing               as Spacing
import qualified XMonad.Layout.Tabbed                as Tabbed

import qualified XMonad.Actions.CycleWS              as CycleWS
import           XMonad.Actions.CycleWindows
import qualified XMonad.Actions.GroupNavigation      as GroupNavigation
import qualified XMonad.Actions.SpawnOn              as SpawnOn
import qualified XMonad.Actions.WorkspaceNames       as WorkspaceNames

import qualified XMonad.Util.Cursor                  as Cursor
import qualified XMonad.Util.ExtensibleState         as ExtState
import qualified XMonad.Util.EZConfig                as EZConfig
import qualified XMonad.Util.Paste                   as Paste
import qualified XMonad.Util.WorkspaceCompare        as WorkspaceCompare

import           Color
import qualified Mode
import           Polybar
import           Utils

-- * Features
-- ** Polybar
polybarPP :: X PP
polybarPP =
    namedGotoPP $
    onedarkPP

polybarUnfocusedPP :: X PP
polybarUnfocusedPP =
  namedGotoPP $
  onedarkUnfocusedPP

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
    , ppExtras           = [hiddenWindows, currentMode]
    , ppOutput           = const $ return ()
    }
    where
      layoutFirstOrder (workspaces : layout : title : extras) =
        [layout] ++ extras ++ [workspaces, title]
      layoutFirstOrder other = other

      -- show currently active mode
      currentMode :: X (Maybe String)
      currentMode = do
        (Mode.ActiveMode mode) <- ExtState.get
        return $
          Polybar.format [ Foreground onedarkBlue ] <$>
          show <$> mode

      -- show number of hidden windows while in 'myTall' layout
      hiddenWindows :: X (Maybe String)
      hiddenWindows = do
        workspace <- gets (W.workspace . W.current . windowset)
        let layout = W.layout workspace
        WindowLimitActive isLimitActive <- ExtState.get
        let numActualWindows = length $ W.integrate' $ W.stack workspace
        let numHiddenWindows =
              (\n -> if n <= 0 then "" else ":" <> show n) $
              numActualWindows - myWindowLimit
        return $
          when_ (description layout == description myTall && isLimitActive) $
          Polybar.format [ Foreground onedarkYellow ] $
          printf "(H%s)" numHiddenWindows

onedarkUnfocusedPP :: PP
onedarkUnfocusedPP = onedarkPP
  { ppCurrent = fromJust $ ppVisibleNoWindows onedarkPP
  , ppVisible = ppHiddenNoWindows onedarkPP
  , ppVisibleNoWindows = Just $ ppHiddenNoWindows onedarkPP
  , ppHidden = ppHiddenNoWindows onedarkPP
  }


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

-- ** Prompt

blackWhitePrompt :: Prompt.XPConfig
blackWhitePrompt = def
    { -- Look
      Prompt.font = "xft:ypn envypn:pixelsize=15"
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
    , Prompt.autoComplete = Nothing
    , Prompt.completionKey = (0, xK_Tab)
    , Prompt.maxComplRows = Just 5
    }

-- ** Workspaces

wsTodo  = "\xf00b"
wsConf  = "\xf992"
wsEntt  = "\xf880"
wsMusic = "\xf001"
wsComms = "\xf086"

myWorkspaces =
    [wsTodo] ++ fmap show [1..7] ++ [wsConf, wsEntt, wsMusic, wsComms]

-- * Startup

myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    EZConfig.checkKeymap myConfig myKeymap
    Cursor.setDefaultCursor Cursor.xC_left_ptr
    spawn "pgrep unclutter || unclutter --timeout 3"

-- * Layouts

myLayoutHook =
    ManageDocks.avoidStruts $
    Borders.smartBorders $
    myPerWorkspaceLayouts $
    MultiToggle.mkToggle (MultiToggle.single NBFULL) $
    myTall ||| myTabbed ||| myOneBig


layoutDescriptionWidth =
    maximum . fmap length $
        [ description myTall
        , description myTabbed
        , description myOneBig
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

myWindowLimit = 3

myTall :: Decoration.ModifiedLayout Renamed.Rename
          (Decoration.ModifiedLayout LimitWindows.LimitWindows
          (Decoration.ModifiedLayout Spacing.Spacing Dwindle))
          Window
myTall =
    named "Windowed" $
    LimitWindows.limitWindows 3 $
    mySpacing $
    Spiral R Dwindle.CW 1 (4/3)


-- *** Toggle window limit

newtype WindowLimitActive = WindowLimitActive Bool

instance ExtensionClass WindowLimitActive where
  initialValue = WindowLimitActive False

toggleWindowLimit :: X ()
toggleWindowLimit = do
  WindowLimitActive isActive <- ExtState.get
  if isActive
  then LimitWindows.setLimit 500
  else LimitWindows.setLimit myWindowLimit
  ExtState.put $ WindowLimitActive (not isActive)
  refresh


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

-- ** OneBig
myOneBig =
  named "Big" $
  OneBig (3/4) (3/4)

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
    , myModeKeys
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
cmdList = [ ("set max volume", setMaxVolume)
          , ("toggle gaps", toggleGaps)
          , ("refresh", refreshAll)
          ]
  where
    setMaxVolume = do
      volString <- Prompt.inputPrompt blackWhitePrompt "Max Volume"
      fromMaybe mempty $ do
        maxVol <- volString >>= readMaybe
        return $ ExtState.put $ Volume maxVol

    toggleGaps =
      sendMessage Gaps.ToggleGaps
      >> Spacing.toggleWindowSpacingEnabled
      >> Spacing.toggleScreenSpacingEnabled

    refreshAll =
      refresh >> rescreen

myControlKeys :: [(String, X ())]
myControlKeys =
    [ ("M-`", recompileXMonad)
    , ("M-S-C-`", io $ exitWith ExitSuccess)
    , ("M-q", kill)
    , ("M-;", Prompt.xmonadPromptC cmdList blackWhitePrompt)
    ]
    where
      recompileXMonad =
          spawn "notify-send 'Reloading XMonad';\
                \xmonad --recompile && xmonad --restart;\
                \sleep 1; notify-send 'XMonad reloaded'"
          >> refresh

-- ** Modes

myModeKeys =
  (fmap Mode.activate) <$>
  [ ("M-<Down>", vimNavMode)
  ]

-- *** Vim Navigation Mode

vimNavMap :: M.Map (KeyMask, KeySym) (X ())
vimNavMap = uncurry Paste.sendKey <$> M.fromList
  [ ((0, xK_h) , (0, xK_Left))
  , ((0, xK_j) , (0, xK_Down))
  , ((0, xK_k) , (0, xK_Up))
  , ((0, xK_l) , (0, xK_Right))
  ]

vimNavMode :: Mode.Mode
vimNavMode = Mode.Mode
  { Mode.name     = "VimNav"
  , Mode.keymap   = vimNavMap
  , Mode.modeType = Mode.CapturePassthrough (0, xK_Escape)
  }

-- ** Media

newtype Volume = Volume Natural
  deriving (Read,Show)

instance ExtensionClass Volume where
  initialValue = Volume 150
  extensionType = PersistentExtension

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
      -- cycle windows
    , ("M-C-j", windows (W.swapMaster . W.focusDown))
    , ("M-C-k", windows (W.swapMaster . W.focusUp))
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
    , ("M-S-w", toggleWindowLimit)
    , ("M-t", jumpToLayout myTabbed)
    , ("M-p", jumpToLayout myOneBig)
      -- modify spacing, full etc
    , ("M-f", fullscreen)
    , ("M-S-f", sendMessage ManageDocks.ToggleStruts)
    ]
    where
      jumpToLayout :: LayoutClass layout a => layout a -> X ()
      jumpToLayout = sendMessage . JumpToLayout . description

      fullscreen = do
        sendMessage $ MultiToggle.Toggle NBFULL
        sendMessage ManageDocks.ToggleStruts


-- ** Launcher

myLauncherKeys =
    fmap spawn <$>
    [ ("M-u t",   terminal myConfig)
    , ("M-u g",   "firefox")
    , ("M-u r",   "$TERMINAL ranger")
    , ("M-u e",   "emacs")
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
main =
    xmonad $
        Polybar.manage polybarPP polybarUnfocusedPP $
        ewmh $
        myConfig
