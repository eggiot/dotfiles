import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.CycleWS

-- make xmonad work with unity

unityManageHook = composeAll (
  [ manageHook gnomeConfig
  , className =? "Unity-2d-panel" --> doIgnore
  ])

-- config

myConfig = gnomeConfig
  {
    manageHook = unityManageHook
  , modMask = mod4Mask
  , focusFollowsMouse = False
  , terminal = myTerminal
  }

-- keycodes

myKeys = [
    -- system
    ("M-S-l", spawn "gnome-screensaver-command -l")
  , ("M-S-e", spawn "gnome-session-quit --logout")
  , ("M-S-q", spawn "gnome-session-quit --power-off")
    
    -- utilities
  , ("M-a", spawnScript "dmenu")
  , ("M-f", spawn myFileManager)
  , ("M-t", spawn "emacs")
  ]

-- main

main = do
  xmonad $ myConfig `additionalKeysP` myKeys

-- programs

myTerminal = "urxvt -fn xft:Consolas:pixelsize:20:antialias:true:autohinting:true"
myFileManager = myTerminal ++ " -e mc"

-- locations

scriptsDir = "~/scripts/"

-- functions

spawnScript :: MonadIO m => String -> m ()
spawnScript x = spawn $ (scriptsDir ++ x)