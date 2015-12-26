import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.StackSet
import XMonad.Util.Scratchpad
import System.IO

mainModMask = mod4Mask

myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"

main = do
      xmobarProc <- spawnPipe "/usr/bin/xmobar $HOME/.xmobarrc"
      xmonad $ (myConfig xmobarProc) `additionalKeys` 
        [ 
      ----  Power-state keys
        --  ((mainModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((mainModMask .|. shiftMask, xK_w), 
        --    spawn "gksudo -g --message 'Suspend?' pm-suspend")
        --, ((mainModMask .|. shiftMask, xK_e), 
        --    spawn "gksudo -g --message 'Shut down?' poweroff")
        --, ((mainModMask .|. shiftMask, xK_r), 
        --    spawn "gksudo -g --message 'Reboot?' reboot")
        --, ((mainModMask .|. shiftMask, xK_y), 
        --    spawn "gksudo -g --message 'Hibernate?' pm-hibernate")
      ----  Utility keys
          ((mainModMask, xK_d), spawn "gmrun")
        , ((mainModMask, xK_F12), scratchpadSpawnActionTerminal "gnome-terminal")
        , ((mainModMask, xK_p), spawn myLauncher)
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
