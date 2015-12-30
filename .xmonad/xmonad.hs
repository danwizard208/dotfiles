import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.StackSet
import XMonad.Util.Scratchpad
import System.IO

mainModMask = mod4Mask

myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"

main = do
  xmobarProc <- spawnPipe "/usr/bin/xmobar $HOME/.xmobarrc"
  xmonad $ (myConfig xmobarProc) `additionalKeysP` 
    [ 
  --  Power-state keys
      ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-S-z", spawn "gksudo -g --message 'Suspend?' pm-suspend")
    , ("M-S-e", spawn "gksudo -g --message 'Shut down?' poweroff")
    , ("M-S-r", spawn "gksudo -g --message 'Reboot?' reboot")
    , ("M-S-y", spawn "gksudo -g --message 'Hibernate?' pm-hibernate")

  --  Utility keys
    , ("M-<F12>", scratchpadSpawnActionCustom "gnome-terminal --name scratchpad")
    --, ((controlMask, xK_Print), 
    --    spawn "sleep 0.2; scrot -s -e \"mv \\$f ${SCREENSHOTS_DIR}\"")
    --, ((0, xK_Print), spawn "scrot -e \"mv \\$f ${SCREENSHOTS_DIR}\"")
  ----  Banshee keys
    --, ((controlMask .|. shiftMask, xK_Right),  
    --    spawn "banshee --next")
    --, ((controlMask .|. shiftMask, xK_Left),   
    --    spawn "banshee --restart-or-previous")
    --            "notify-send Banshee\\ `banshee --query-volume`")
    --, ((controlMask .|. shiftMask, xK_Down),   
    --    spawn $ "banshee --set-volume=-10;"++
    --            "notify-send Banshee\\ `banshee --query-volume`")
    --, ((controlMask .|. shiftMask, xK_space),  
    --    spawn "banshee --toggle-playing")
    --, ((controlMask .|. shiftMask, xK_Return), spawn "banshee --stop")
    --, ((controlMask .|. shiftMask, xK_m),      spawn "banshee --show")
    ]

myConfig xmobarProc = defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , modMask = mod4Mask
        , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmobarProc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
        , focusFollowsMouse = False
        } 

myManageHook = composeAll
    [
      className =? "Xmessage" --> doFloat
    , manageDocks
    , manageScratchPad
    ] 

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (RationalRect l t w h)
    where           --All below are fractions of screen size
        h = 0.4     --terminal height
        w = 1       --terminal width
        t = 0       --distance from top edge
        l = 1 - w   --distance from left edge
