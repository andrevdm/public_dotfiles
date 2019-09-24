{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/randomthought/xmonad-config
-- http://www.gitlab.com/dwt1/

import Protolude hiding ((<&&>))
import qualified Data.List as Lst

    -- Base
import           XMonad
import           XMonad.Config.Desktop
import           Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import           System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import           XMonad.Prompt.ConfirmPrompt (confirmPrompt)

    -- Utilities
import qualified XMonad.Util.Cursor as UCur
import qualified XMonad.Util.Loggers as ULog
import           XMonad.Util.EZConfig (mkKeymap, additionalKeysP, additionalMouseBindings, removeKeysP)
import qualified XMonad.Util.NamedScratchpad as UNScr
import           XMonad.Util.NamedScratchpad (NamedScratchpad (..), namedScratchpadManageHook, namedScratchpadAction)
import           XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import qualified XMonad.Util.SpawnOnce as USOne
import           XMonad.Util.SpawnOnce (spawnOnce)

    -- Hooks
import           XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import           XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doSideFloat, Side(..)) 
import           XMonad.Hooks.Place (placeHook, withGaps, smart)
import qualified XMonad.Hooks.SetWMName as HkSwn
import qualified XMonad.Hooks.EwmhDesktops as EDsk   -- required for xcomposite in obs to work

    -- Actions
import           XMonad.Actions.UpdatePointer (updatePointer)
import qualified XMonad.Actions.Navigation2D as N2D
import           XMonad.Actions.Minimize (minimizeWindow)
import qualified XMonad.Actions.Promote as ActP
import           XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import           XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import           XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import           XMonad.Actions.WithAll (sinkAll, killAll)
import           XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), shiftNextScreen, shiftPrevScreen) 
import           XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import           XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import qualified XMonad.Actions.MouseResize as ActMR
import           XMonad.Actions.MouseResize (mouseResize)
import qualified XMonad.Actions.ConstrainedResize as Sqr
import qualified XMonad.Actions.PhysicalScreens as Phs


    -- Layouts modifiers
import           XMonad.Layout.PerWorkspace (onWorkspace) 
import           XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import qualified XMonad.Layout.Decoration as LDec
import qualified XMonad.Layout.WorkspaceDir as LWsDir
import           XMonad.Layout.Spacing (spacing) 
import qualified XMonad.Layout.NoBorders as LayNBd
import           XMonad.Layout.NoBorders (noBorders)
import qualified XMonad.Layout.BinarySpacePartition as BSP
import           XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import           XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import           XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import           XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import           XMonad.Layout.GridVariants (Grid(Grid))
import qualified XMonad.Layout.SimplestFloat as LaySFloat
import           XMonad.Layout.SimplestFloat (simplestFloat)
import qualified XMonad.Layout.OneBig as LayOneB
import           XMonad.Layout.OneBig (OneBig (..))
import qualified XMonad.Layout.ThreeColumns as LayThreeCol
import           XMonad.Layout.ThreeColumns (ThreeCol (..))
import qualified XMonad.Layout.ResizableTile as LayRTile
import qualified XMonad.Layout.Gaps as Gaps
import           XMonad.Layout.ResizableTile (ResizableTall (..))
import           XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import           XMonad.Layout.IM (withIM, Property(Role))
import           XMonad.Layout.Tabbed (simpleTabbed)

    -- Prompts
import           XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

--import qualified Data.Map.Strict as Map
import qualified Data.Monoid
import           Data.Default (def)



myTerminal :: [Char]
myTerminal = "termite"
--myTerminal = "alacritty"

myLauncher :: [Char]
myLauncher = "rofi -combi-modi window,drun -show combi -modi combi"

myTextEditor :: [Char]
myTextEditor = "nvim"     -- Sets default text editor


main :: IO ()
main = do
  wsbar <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
  
  xmonad $ EDsk.ewmh desktopConfig
    { borderWidth = 2
    , focusFollowsMouse = True
    --, keys = \c -> mkKeymap c myKeys
    , layoutHook = myLayoutHook
    , logHook = myLogHook wsbar >> updatePointer (0.5,0.5) (0,0)
    , manageHook = (isFullscreen --> doFullFloat) <> myManageHook <> manageHook desktopConfig <> manageDocks
    , modMask = mod4Mask
    , mouseBindings = myMouseBindings
    , startupHook = myStartupHook
    , terminal = myTerminal
    , workspaces = myWorkspaces

    , normalBorderColor  = "#292d3e"
    , focusedBorderColor = "#bbc5ff"
    }
    `additionalKeysP` myKeys
    `removeKeysP`
       [ "M-S-p"
       , "M-S-c"
       ]

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  HkSwn.setWMName "LG3D"
  spawnOnce "bash ~/.xmonad/startup.sh"

  -- Write a space to the base pipe so that xmobar will hide the log pipe
  --  The xmobar config should look like this
  --
  --      Run BufferedPipeReader "mylog"
  --      [ (  0, False, "/tmp/xmonad.pipe.base"  )
  --      , ( 20, False, "/tmp/xmonad.pipe.log" )
  --      ]
  --
  -- And in the startup.sh above you need
  --
  --     mkfifo /tmp/xmonad.pipe.log
  --     mkfifo /tmp/xmonad.pipe.base
  --
  -- You can then write log messages to /tmp/xmonad.pipe.log which will get show for a short period on the
  --  xmobar before being cleared
  spawn "echo ' ' > /tmp/xmonad.pipe.base" 

  UCur.setDefaultCursor UCur.xC_left_ptr

  
------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------
myWorkspaces :: [[Char]]
myWorkspaces = (show <$> [1..9]) <> ["0"]

  
------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
  -- Xmonad
  [ ("M-C-r", spawn "bash -c \"xmonad --recompile 2>&1 | tail -n 1 > /tmp/xmonad.pipe.log \"" )      -- Recompiles xmonad
  , ("M-S-r", spawn "echo 'Compiling...' > /tmp/xmonad.pipe.log && xmonad --recompile && xmonad --restart")  -- Restarts xmonad
  , ("M-S-e", confirmPrompt defaultXPConfig "exit" $ io exitSuccess)   -- Quits xmonad

  -- System
  , ("M-C-M1-l", spawn "xautolock -locknow")
    
  -- Windows
  , ("M-S-q", kill1)                           -- Kill the currently focused client
  , ("M-S-a", killAll)                         -- Kill all the windows on current workspace

  -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
  , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.

  -- Windows navigation
  , ("M-m", windows W.focusMaster)             -- Move focus to the master window
  , ("M-j", windows W.focusDown)               -- Move focus to the next window
  , ("M-k", windows W.focusUp)                 -- Move focus to the prev window
  , ("M-S-m", windows W.swapMaster)            -- Swap the focused window and the master window
  , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
  , ("M-<Backspace>", ActP.promote)                 -- Moves focused window to master, all others maintain order
  , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
  , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
  , ("M-S-s", windows copyToAll)  
  , ("M-C-s", killAllOtherCopies) 
      
  , ("M-C-M1-<Up>", sendMessage Arrange)
  , ("M-C-M1-<Down>", sendMessage DeArrange)
  , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
  , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
  , ("M-<Right>", sendMessage (MoveRight 10))       --  Move focused window to right
  , ("M-<Left>", sendMessage (MoveLeft 10))         --  Move focused window to left
  , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
  , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
  , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
  , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
  , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
  , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
  , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
  , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left
  
  -- Layouts
  , ("M-<Space>", sendMessage NextLayout)                              -- Switch to next layout
  , ("M-S-<Space>", sendMessage ToggleStruts)                          -- Toggles struts
  , ("M-S-b", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
  , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-S-f", sendMessage (T.Toggle "float"))
  , ("M-S-x", sendMessage $ Toggle REFLECTX)
  , ("M-S-y", sendMessage $ Toggle REFLECTY)
  , ("M-S-m", sendMessage $ Toggle MIRROR)
  , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
  , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
  , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
  , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

  , ("M-C-h", sendMessage Shrink)
  , ("M-C-l", sendMessage Expand)
  , ("M-C-j", sendMessage LayRTile.MirrorShrink)
  , ("M-C-k", sendMessage LayRTile.MirrorExpand)
  , ("M-S-;", sendMessage zoomReset)
  , ("M-;", sendMessage ZoomFullToggle)

  -- Workspaces
  --, ("M-0", windows $ W.view "0")                                     -- Go to workspace 0
  , ("M-<KP_Add>", moveTo Next nonNSP)                                -- Go to next workspace
  , ("M-<KP_Subtract>", moveTo Prev nonNSP)                           -- Go to previous workspace
  , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
  , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

  -- Scratchpads
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-m", namedScratchpadAction myScratchPads "cmus")
  , ("M-C-c", namedScratchpadAction myScratchPads "qalc")
  , ("M-C-o", namedScratchpadAction myScratchPads "org")
  --, ("M-C-s", namedScratchpadAction myScratchPads "slack")
      
  -- Main Run Apps
  , ("M-<Return>", spawn myTerminal)
  , ("M-d", spawn myLauncher)
    
  -- Multimedia Keys
  , ("M-C-S-<Delete>", spawn "amixer set Master 0% unmute")
  , ("M-C-S-<Up>", spawn "amixer set Master 5%+ unmute")
  , ("M-C-S-<Down>", spawn "amixer set Master 5%- unmute")
  , ("M-C-S-<End>", spawn "cmus-remote --pause; ~/.xmonad/get-cmus-current.sh > /tmp/xmonad.pipe.log")
  , ("M-C-S-<Left>", spawn "cmus-remote --prev; ~/.xmonad/get-cmus-current.sh > /tmp/xmonad.pipe.log")
  , ("M-C-S-<Right>", spawn "cmus-remote --next; ~/.xmonad/get-cmus-current.sh > /tmp/xmonad.pipe.log")
  
  , ("<XF86AudioPlay>", spawn "cmus-remote --pause")
  , ("<XF86AudioPrev>", spawn "cmus-remote --prev; ~/.xmonad/get-cmus-current.sh > /tmp/xmonad.pipe.log")
  , ("<XF86AudioNext>", spawn "cmus-remote --next; ~/.xmonad/get-cmus-current.sh > /tmp/xmonad.pipe.log")
  -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
  , ("<XF86HomePage>", spawn "firefox")
  , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
  , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
  , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
  , ("<XF86Eject>", spawn "toggleeject")

  -- Screenshot, notice sleep 0.2 to make this work. See https://bbs.archlinux.org/viewtopic.php?id=86507
  -- , ("<Print>", spawn "sleep 0.2; scrot -s 'scrot_%Y-%m-%d_%H-%M_$wx$h.png' -e 'mv $f ~/temp'")
  , ("<Print>", spawn "flameshot gui -p /home/andre/temp")

  ] <> (myWorkspaces <&> (\c -> ("M-" <> c, windows $ W.view c))) -- Go to workspace 0, non-greedy
  -- <>
  -- [((modm .|. mask, key), f sc)
  --   | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  --   , (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]

  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

------------------------------------------------------------------------
-- Mouse bindings
------------------------------------------------------------------------
myMouseBindings :: XConfig l -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = Map.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ appName   =? "desktop_window"               --> doIgnore
  , className =? "Galculator"                   --> doCenterFloat
  , className =? "Steam"                        --> doCenterFloat
  , className =? "Gimp"                         --> doCenterFloat
  , appName   =? "gpicview"                     --> doCenterFloat
  , className =? "MPlayer"                      --> doCenterFloat
  , className =? "Pavucontrol"                  --> doCenterFloat
  , className =? "Mate-power-preferences"       --> doCenterFloat
  , className =? "Xfce4-power-manager-settings" --> doCenterFloat

  --WM_NAME = 
  --, className =? "Git-gui"                      --> (doF W.focusUp <> doFloat)
  , stringProperty "WM_NAME" =^? "Git Gui ("     --> doCenterFloat

  --, className =? "VirtualBox"                   --> doShift "4:vm"
  --, className =? "Xchat"                        --> doShift "5:media"
  , className =? "stalonetray"                  --> doIgnore
  --, className =? "Google-chrome"                --> doShift "2:web"
  , isFullscreen                                --> (doF W.focusDown <> doFullFloat)
  -- , isFullscreen                             --> doFullFloat
  ] <> namedScratchpadManageHook myScratchPads


(=^?) :: (Eq a) => Query [a] -> [a] -> Query Bool
q =^? x = fmap (Lst.isPrefixOf x) q

------------------------------------------------------------------------
-- Named scrachpads
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus  
                , NS "org" spawnOrg findOrg manageOrg
                , NS "qalc" spawnQalc findQalc manageQalc
                --, NS "slack" spawnSlack findSlack manageSlack
                ]

  where
    spawnOrg  = "emacs --name org.todo ~/Dropbox/org/hyrax.org"
    findOrg   = title =? "org.todo"
    manageOrg = UNScr.customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    spawnTerm  = myTerminal <> " -t scratchpad -e tmux"
    findTerm   = title =? "scratchpad"
    manageTerm = UNScr.customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    spawnCmus  = myTerminal <> " -t cmus -e cmus"
    findCmus   = title =? "cmus"
    manageCmus = UNScr.customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    spawnQalc  = myTerminal <> " -t qalc -e qalc"
    findQalc   = title =? "qalc"
    manageQalc = UNScr.customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.5
        t = 0.95 -h
        l = 0.75 -w

    --spawnSlack  = "/usr/bin/slack"
    --findSlack   = stringProperty "WM_CLASS" =^? "Slack"
    --manageSlack = UNScr.customFloating $ W.RationalRect l t w h
    --  where
    --    h = 0.9
    --    w = 0.9
    --    t = 0.95 -h
    --    l = 0.95 -w


  
--------------------------------------------------------------------------- 
-- myLogHook:         loghock settings                                     
-------------------------------------------------------------------------------
myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }


--------------------------------------------------------------------------- }}}
-- myWsBar:           xmobar setting                                        {{{
-------------------------------------------------------------------------------
wsPP = xmobarPP { ppOrder           = \(ws:l:t:_)  -> [ws,l,t]
                --, ppCurrent         = xmobarColor colorRed     colorNormalbg . \s -> "●"
                --, ppUrgent          = xmobarColor colorGray    colorNormalbg . \s -> "●"
                --, ppVisible         = xmobarColor colorRed     colorNormalbg . \s -> "⦿"
                --, ppHidden          = xmobarColor colorGray    colorNormalbg . \s -> "●"
                --, ppHiddenNoWindows = xmobarColor colorGray    colorNormalbg . \s -> "○"
                --, ppTitle           = xmobarColor colorRed     colorNormalbg
                , ppOutput          = putStrLn
                , ppWsSep           = " "
                , ppSep             = "  "
                }


------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
outerGaps    = 1
myGaps       = Gaps.gaps [(BSP.U, outerGaps), (BSP.R, outerGaps), (BSP.L, outerGaps), (BSP.D, outerGaps)]

myLayoutHook =
  avoidStruts
  $ mouseResize
  $ windowArrange
  $ T.toggleLayouts floats
  $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
  $ myGaps
  $ myDefaultLayout

  where 
    myDefaultLayout = tall ||| grid ||| tabbed ||| threeCol ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats


tabbed     = renamed [Replace "tabbed"]   $ limitWindows 12 $ simpleTabbed
tall       = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol   = renamed [Replace "threeCol"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2) 
threeRow   = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "oneBig"]   $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 $ Full
space      = renamed [Replace "space"]    $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "floats"]   $ limitWindows 20 $ simplestFloat
