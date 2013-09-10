import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig(additionalKeysP)

-- make xmonad work with unity

unityManageHook = composeAll (
  [
    manageHook gnomeConfig,
    className =? "Unity-2d-panel" --> doIgnore
  ])

-- config

myConfig = gnomeConfig
  {
    manageHook = unityManageHook,
    modMask = mod4Mask,
    focusFollowsMouse = False,
    terminal = terminalCmd
  }

-- key bindings

myKeys = [
    -- system
  appKeyBind "M-S-l" "gnome-screensaver-command -l",
  appKeyBind "M-S-e" "gnome-session-quit --logout",
  appKeyBind "M-S-q" "gnome-session-quit --power-off",
    
  -- utilities
  scriptKeyBind "M-x" "dmenu",
  appKeyBind "M-f" fileCmd,
  appKeyBind "M-b" textCmd,
  appKeyBind "M-u" browserCmd
  ]

-- main

main = do
  xmonad $ myConfig `additionalKeysP` myKeys

-- applications

terminalCmd = "urxvt -fn xft:Consolas:size=16:antialias=true:autohinting=true"
fileCmd = terminalCmd ++ " -e mc"
browserCmd = "firefox --app /home/eliot/bin/conkeror/application.ini"
textCmd = "subl"

-- locations
scriptDir = "~/scripts/"

-- functions

appKeyBind :: String -> String -> (String, X())
appKeyBind k s = (k, spawn s)

scriptKeyBind :: String -> String -> (String, X())
scriptKeyBind k s = appKeyBind k (getScript s)

getScript :: String -> String
getScript x = scriptDir ++ x