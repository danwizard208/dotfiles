import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import System.IO
import Data.Ratio ((%))

mainModMask = mod4Mask

main = do
  xmobarProc <- spawnPipe "/usr/bin/xmobar $HOME/.xmobarrc"
  xmonad $ (myConfig xmobarProc) `additionalKeysP`
    [
  --  Power-state keys
      ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-S-z", spawn "systemctl suspend")
    -- , ("M-S-y", spawn "sudo pm-hibernate")
    , ("M-S-e", spawn "gksudo -g --message 'Shut down?' poweroff")
    , ("M-S-r", spawn "gksudo -g --message 'Reboot?' reboot")

  --  Utility keys
    , ("M-<F12>", namedScratchpadAction myScratchPads "terminal")
    , ("M-b", sendMessage ToggleStruts)
    , ("M-p", spawn "dmenu_run -fn \
            \'DroidSansMonoForPowerline Nerd Font 16' -nf 'green' -sf 'green'")
    ]

myTerminal = "termite"

myWorkspaces =
    (map show [1..6]) ++
    ["7","8-Pidgin","9"]

myScratchPads = [
    NS "terminal" spawnTerm findTerm manageTerm
    ]
    where
        spawnTerm = myTerminal ++ " --role=scratchpad"
        findTerm = stringProperty "WM_WINDOW_ROLE" =? "scratchpad"
        manageTerm = customFloating $ W.RationalRect l t w h
            where           --All below are fractions of screen size
                h = 0.4     --terminal height
                w = 1       --terminal width
                t = 0       --distance from top edge
                l = 1 - w   --distance from left edge

myConfig xmobarProc = defaultConfig
        { workspaces = myWorkspaces
        , terminal = myTerminal
        , manageHook = myManageHook <+> manageHook defaultConfig
        , handleEventHook = myHandleEventHook <+> handleEventHook defaultConfig
        , layoutHook = myLayoutHook
        , modMask = mod4Mask
        , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmobarProc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
        , focusFollowsMouse = False
        }

myLayoutHook = layoutHintsToCenter $ avoidStruts $
    onWorkspace "7" skypeLayout $
    onWorkspace "8-Pidgin" imLayout $
    standardLayouts

standardLayouts =
     myTallLayout
     ||| Mirror myTallLayout
     ||| noBorders simpleTabbed
     ||| noBorders Full

imLayout = withIM (1/5) pidginMainWindow standardLayouts
    where
        pidginMainWindow =
            (Role "buddy_list")

skypeLayout = withIM (1/5) skypeMainWindow standardLayouts
    where
        skypeMainWindow =
            (And (Resource "skype")
                 (Not (Role "ConversationsWindow")))

myTallLayout = Tall nmaster delta ratio
    where
        -- Default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 2/100
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2

myManageHook = composeAll
    [
      className =? "Xmessage" --> doFloat
    , resource =? "skype"    --> doShift "7"  -- Force to Skype workspace.
    , manageDocks
    , manageScratchPad
    ]

myHandleEventHook = composeAll
    [
      hintsEventHook
    ]

manageScratchPad :: ManageHook
manageScratchPad = namedScratchpadManageHook myScratchPads
