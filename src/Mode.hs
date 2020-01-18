{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mode  where

import           XMonad

import           XMonad.Actions.Submap

import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Paste           as Paste

import qualified Data.Map                    as M
import           Data.Maybe

import           Text.Printf



type Key = (KeyMask, KeySym)

-- * Mode state

-- |
-- 'CapturePassthrough' triggers unbound keys in the underlying keymap,
--   and sends them to the to focused window if unbound even there.
-- 'Passthrough' sends unbound keys through to the focused window, but does not trigger associated X actions.
-- 'IgnoreUnbound' completely ignores unbound keys.
-- 'ExitOnUnbound' exits the mode on the first unbound input and gobbles it.
data ModeType
  = CapturePassthrough (M.Map Key (X ())) Key
  | Passthrough Key
  | IgnoreUnbound Key
  | ExitOnUnbound

data Mode = Mode
  { name     :: String
  , keymap   :: M.Map Key (X ())
  , modeType :: ModeType
  }

newtype ActiveMode = ActiveMode (Maybe Mode)

instance ExtensionClass ActiveMode where
  initialValue = ActiveMode Nothing


-- * Mode keymap

-- | A persistent submap.
hydra :: Mode -> X ()
hydra mode = case modeType mode of
  ExitOnUnbound ->
    submapDefault
      deactivate
      (persisting <$> keymap mode)

  IgnoreUnbound quitKey ->
    submapDefault
      (hydra mode)
      (M.insert quitKey deactivate $ persisting <$> keymap mode)

  Passthrough quitKey ->
    submapDefaultWithKey
      (persisting . uncurry Paste.sendKey)
      (M.insert quitKey deactivate $ persisting <$> keymap mode)

  CapturePassthrough baseMap quitKey ->
    submapDefaultWithKey
      (\key -> persisting $ fromMaybe (uncurry Paste.sendKey key) (M.lookup key baseMap))
      (M.insert quitKey deactivate $ persisting <$> keymap mode)

  where
    persisting action = action >> hydra mode
    deactivate = XS.remove (ActiveMode $ Just mode) >> refresh


-- * Mode activation

activate :: Mode -> X ()
activate mode = do
  XS.put $ ActiveMode (Just mode)
  refresh
  hydra mode


-- * Printing (modeline)

instance Show ModeType where
  show ExitOnUnbound            = "Exit"
  show (IgnoreUnbound _)        = "Ign"
  show (Passthrough _)          = "pass"
  show (CapturePassthrough _ _) = "Pass"

instance Show Mode where
  show (Mode name _ modeType) =
    printf "%s(%s)" name (show modeType)
