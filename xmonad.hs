--------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
--
-- Author: Chuck Valenza
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------
import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn

import Control.Monad

import Data.Ratio((%))
import Data.Char

import Foreign.C.Types (CInt)
import Graphics.X11.Xinerama

import System.IO.UTF8

import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive(setOpacity)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run

--------------------------------------------------------------------------------
-- XMonad
--------------------------------------------------------------------------------

main = do
 screenWidth <- getScreenWidth getScreenNumber
 screenHeight <- getScreenHeight getScreenNumber
 statusbar <- spawnPipe ( myStatusBar screenWidth screenHeight )
 time <- spawnPipe ( myTimeBar screenWidth screenHeight )
 volume <- spawnPipe ( myVolBar screenWidth screenHeight )
 username <- spawnPipe ( myUserBar screenWidth )
 comkybat <- spawnPipe ( myBatBar screenWidth )
 comkympd <- spawnPipe ( myMPDBar screenWidth )
 conkycpu <- spawnPipe ( myCPUBar screenWidth )
 conkyram <- spawnPipe ( myRAMBar screenWidth )
 conkyuptime <- spawnPipe ( myUPBar screenWidth )
 xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
   { terminal     			= myTerminal
   , workspaces 	  		= myWorkspaces
   , manageHook 			  = myManageHook <+> manageDocks <+> manageSpawn
   , layoutHook 			  = myLayout
   , borderWidth        = myBorderWidth
   , logHook 	 			    = dynamicLogWithPP $ myDzenPP statusbar
   , focusedBorderColor	= myFocusedBorderColor
   , normalBorderColor 	= myNormalBorderColor
   , modMask	 			    = mod4Mask -- modkey is now Windows Key
   } `additionalKeys`
   [ ((mod4Mask .|. shiftMask, xK_b), spawn "chromium")
   , ((mod4Mask .|. shiftMask, xK_i), spawn "urxvt -tr -e irssi -c 132.177.4.36 -p 45680 -w coolwhip:coolwhip")
   , ((mod4Mask .|. shiftMask, xK_f), spawn "urxvt -tr -e fbchat")
   , ((mod4Mask,               xK_s), spawn "lockscript")
   , ((mod4Mask,               xK_m), spawn "urxvt +tr -e ncmpcpp")
   , ((mod4Mask .|. shiftMask, xK_m), spawn "spotify")
   , ((mod4Mask,               xK_i), spawn "eog")
   , ((mod4Mask,               xK_w), spawn "wpa_gui")
   , ((mod4Mask,               xK_p), shellPromptHere myLauncherConfig)
   , ((mod4Mask,            xK_Left), prevWS)
   , ((mod4Mask,           xK_Right), nextWS)
   , ((mod1Mask,              xK_F4), kill)
   , ((mod4Mask,           xK_Print), spawn "scrot -e 'mv $f ~/Pictures/screenshots/'")
   , ((mod4Mask .|. shiftMask, xK_p), spawn "mpc toggle") -- play/pause song
   , ((mod4Mask,            xK_Home), spawn "mpc prev") -- previous song
   , ((mod4Mask,             xK_End), spawn "mpc next") -- next song
   , ((mod4Mask,           xK_minus), spawn "~/.xmonad/scripts/.brightness_modifier -2") -- brightness down
   , ((mod4Mask,           xK_equal), spawn "~/.xmonad/scripts/.brightness_modifier 2") -- brightness up
   , ((mod4Mask,             xK_Tab), spawn "~/.xmonad/scripts/.touchpad_toggle") -- toggle touchpad on/off
   , ((0, 0x1008FF2A), spawn "poweroff")
   , ((0, 0x1008FF11), spawn "amixer set Master 3-")
   , ((0, 0x1008FF13), spawn "amixer set Master 2+")
   , ((0, 0x1008FF12), spawn "amixer set Master toggle")
   ]
-------------------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------------------
myTerminal           = "urxvt -tr"
myIconDir            = "/icons"
myBorderWidth        = 2
myNormalBorderColor  = "#0f0f0f"
myFocusedBorderColor = "#0099ff"
myFont               = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- Set workspace names
myWorkspaces = [ " 1 "
               , " 2 "
               , " 3 "
               , " 4 "
               , " 5 "
               , " 6 "
               , " 7 "
               , " 8 "
               , " chat "
               ]

cyan      = "#0099ff" --Cyan
lightGray = "#333333" --Light Grey
white1    = "#ffffff" --Bright White
white2    = "#f0f0f0" --White 2
black     = "#0f0f0f" --Black
yellow    = "#fff000" --Yellow
red       = "#d42020" --red
darkGray  = "#222222" --Dark Grey

-- XmonadPrompt config:
myLauncherConfig = defaultXPConfig
   { font = "" ++ myFont ++ ""
   , bgColor = "" ++ black ++ ""
   , fgColor = "" ++ white1 ++ ""
   , fgHLight = "" ++ white1 ++ ""
   , bgHLight = "" ++ darkGray ++ ""
   , borderColor = "" ++ myFocusedBorderColor ++ ""
   , promptBorderWidth = 1
   , position = Bottom
   , height = 16
   , historySize = 100
   }

-- dynamicLog pretty printer for dzen (prints workspace and window info):
myDzenPP h = defaultPP
   { ppOutput  = System.IO.UTF8.hPutStrLn h
   , ppSep     = " "
   , ppWsSep   = ""
   , ppCurrent = wrap("^fg(" ++ cyan ++ ")^bg(" ++ lightGray ++ ")") ("^fg()^bg()")
   , ppUrgent  = wrap("^fg(" ++ darkGray ++ ")^bg(" ++ red ++ ")") ("^fg()^bg()")
   , ppVisible = wrap("^fg(" ++ white1 ++ ")^bg(" ++ darkGray ++ ")") ("^fg()^bg()")
   , ppHidden	 = wrap ("^fg(" ++ white1 ++ ")^bg(" ++ darkGray ++ ")") ("^fg()^bg()")
   , ppHiddenNoWindows	=	wrap ("^fg(" ++ white1 ++ ")^bg(" ++ black ++ ")") ("^fg()^bg()")
   , ppLayout  = dzenColor white1 black .
	               (\x -> case x of
                     "Spacing 1 Tall"         ->  ""
                     "Spacing 1 Mirror Tall"  ->  ""
                     "Spacing 1 Full"         ->  ""
                     _                        ->  ""
                  )
}

-- Retrieve the width dimension of the primary screen
getScreenWidth s = do
   dsp <- openDisplay ""
   mss <- xineramaQueryScreens dsp
   return $ case mss of
      Nothing -> 0
      Just [] -> 0
      Just ss -> if s >= 0 && s < length ss -- prevent bad index
         then fromIntegral . xsi_width $ ss !! s else 0

-- Retrieve the height dimension of the primary screen
getScreenHeight s = do
   dsp <- openDisplay ""
   mss <- xineramaQueryScreens dsp
   return $ case mss of
      Nothing -> 0
      Just [] -> 0
      Just ss -> if s >= 0 && s < length ss -- prevent bad index
         then fromIntegral . xsi_height $ ss !! s else 0

getScreenNumber = 0

--Bottom bar sizes and locations
myVolBarX      screenWidth  = show ( screenWidth - 76 )
myTimeBarX     screenWidth  = show ( screenWidth - 256 ) --myStatusBar width is the same value as myTimeBarX
myBottomBarY   screenHeight = show ( screenHeight - 16 )
--top bar sizes and locations
myBatBarX      screenWidth  = show ( screenWidth - 100 )
myCPUBarX      screenWidth  = show ( screenWidth - 215 )
myRAMBarX      screenWidth  = show ( screenWidth - 328 )
myUPBarX       screenWidth  = show ( screenWidth - 467 )
myMPDBarWidth  screenWidth  = show ( screenWidth - 667 )

-- Statusbars:
  -- bottom (right to left)
myVolBar    screenWidth screenHeight = "conky -c ~/.xmonad/scripts/.volmon | dzen2 -x '" ++ myVolBarX screenWidth ++ "' -y '" ++ myBottomBarY screenHeight ++ "' -h '16' -w '80' -ta 'l' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"
myTimeBar   screenWidth screenHeight = "exec ~/.xmonad/scripts/.date | dzen2 -x '" ++ myTimeBarX screenWidth ++ "' -y '" ++ myBottomBarY screenHeight ++ "' -h '16' -w '180' -ta 'c' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn " ++ myFont ++ " -l 7 -sa 'c' -u -e 'button1=uncollapse;leavetitle=collapse;button2=;'"
myStatusBar screenWidth screenHeight = "dzen2 -x '0' -y '" ++ myBottomBarY screenHeight ++ "' -h '16' -w '" ++ myTimeBarX screenWidth ++ "' -ta 'l' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn " ++ myFont ++ " -p -e 'button2=;'"
  -- top (right to left)
myBatBar    screenWidth = "conky -c ~/.xmonad/scripts/.batmon | dzen2 -x '" ++ myBatBarX screenWidth ++ "' -y '0' -h '16' -w '100' -ta 'l' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"
myCPUBar    screenWidth = "conky -c ~/.xmonad/scripts/.cpumon | dzen2 -x '" ++ myCPUBarX screenWidth ++ "' -y '0' -h '16' -w '115' -ta 'l' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"
myRAMBar    screenWidth = "conky -c ~/.xmonad/scripts/.rammon | dzen2 -x '" ++ myRAMBarX screenWidth ++ "' -y '0' -h '16' -w '113' -ta 'l' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"
myUPBar     screenWidth = "conky -c ~/.xmonad/scripts/.upmon  | dzen2 -x '" ++ myUPBarX screenWidth ++ "' -y '0' -h '16' -w '140' -ta 'c' -fg '" ++ white1 ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"
myMPDBar    screenWidth = "conky -c ~/.xmonad/scripts/.mpdmon | dzen2 -x '200' -y '0' -h '16' -w '" ++ myMPDBarWidth screenWidth ++ "' -ta 'r' -fg '" ++ yellow ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"
myUserBar   screenWidth = "exec ~/.xmonad/scripts/.logindisp  | dzen2 -x '0' -y '0' -h '16' -w '200' -ta 'l' -fg '" ++ cyan ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "' -e 'button2=;'"

--myTheme = defaultTheme
--   { decoHeight          = 16
--   , activeColor         = "#a6c292"
--   , activeBorderColor   = "#a6c292"
--   , activeTextColor     = "#000000"
--   , inactiveBorderColor = "#000000"
--   }

-- Layouts for workspaces
myLayout = avoidStruts $ spacing 2 $ onWorkspace " 8 " gimpLayout $ onWorkspace " chat " chatLayout $ standardLayout
   where
   standardLayout = (tiled ||| Mirror tiled ||| full)

   -- default tiling algorithm partitions the screen into two panes
   tiled   = Tall nmaster delta ratio

   full    = spacing 12 $ Full 

   -- The default number of windows in the master pane
   nmaster = 1

   -- Default proportion of screen occupied by master pane
   ratio   = 65/108

   -- Percent of screen to increment by when resizing panes
   delta   = 3/100

   -- Layout for " 8 " (specifically modified to accomodate Gimp)
   gimpLayout = smartBorders $ withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ spacing 0 $ Full

   -- Layout for " chat "
   chatLayout = reflectHoriz $ withIM (0.17) pidginRoster $ withIM (0.17) steamFriends $ withIM (0.17) skypeRoster (Grid ||| Full)
      where
      steamFriends = And (ClassName "Steam") (Title "Friends")
      skypeRoster  = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindow")) `And` (Not (Role "ConversationsWindow"))
      pidginRoster = (ClassName "Pidgin") `And` (Role "buddy_list")

-- Window rules:
myManageHook = composeAll . concat $
   [ [isDialog --> doFloat]
   , [isFullscreen --> doFullFloat] -- `And` (Not (Title "Win7 Ultimate 64-bit [Running] - Oracle"))]
   , [className =? c --> doFloat | c <- myCFloats]
   , [title =? t --> doFloat | t <- myTFloats]
   , [resource =? i --> doIgnore | i <- myIgnores]
   , [className =? m --> doShift " 8 " | m <- myMShifts]
   , [className =? "Skype" --> doShift " chat "]
   , [className =? "Pidgin" --> doShift " chat "]
   , [title =? ch --> doShift " chat " | ch <- myCShifts]
   , [className =? "Steam" <&&> title /=? "Friends" --> doShift " 8 "]
   ]
   where
   myMShifts = ["Gimp", "Spotify"] -- Media Shifts
   myCShifts = ["irssi", "Friends", "fbchat"] -- Chat Shifts
   myCFloats = ["skype", "calculator", "XFontSel", "Xmessage"] -- Class Floats
   myTFloats = ["Downloads", "Save As...", "Hammerwatch 1.21", "DOOM 3: BFG Edition", "Hotline Miami"] -- Title Floats
   myIgnores = ["desktop_window"]

