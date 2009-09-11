
import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import System.Exit
import System.IO

import Graphics.X11.Xlib

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = do
		dpy <- openDisplay ""
		let dflt = defaultScreen dpy
		let scr = defaultScreenOfDisplay dpy
		let screen_size = widthOfScreen scr

		let myStatusBar = "dzen2  -ta 'l' -x 0 -w " ++ show (div screen_size 2)
		let myTopBar = "conky-cli  -c ~/.xmonad/conky-rc | dzen2 -ta 'r' -x " ++ show (div screen_size 2) ++ " -w " ++ show (div screen_size 2)

		dzen <- spawnPipe myStatusBar
		conkyTop <- spawnPipe myTopBar

		xmonad  defaultConfig 
				{ modMask = myModMask
				, layoutHook = myLayout
				, keys = myKeys
				, logHook = dynamicLogWithPP (myDzenPP dzen)
				, borderWidth = myBorderWidth
				--, manageHook = manageDocks <+> manageHook defaultConfig
				}

-- check out http://haskell.org/haskellwiki/Xmonad/Config_archive/Template_xmonad.hs_(darcs)
-- for the defaults

myBorderWidth = 5

-- Make the mod key be the windows key
myModMask = mod4Mask

-- avoidStruts will make sure not avoid any sort of menu or status bar.
-- smartBorders will only use a border where necessary.
myLayout = avoidStruts $ smartBorders $ layoutHook defaultConfig


-- this is for dzen2
-- myLogHook = dynamicLogDzen

myDzenPP h = defaultPP
	{ ppCurrent	= dzenColor "green" "black" . pad
	, ppVisible	= dzenColor "lightgreen" "" . pad
	, ppHidden	= dzenColor "white" "" . pad
	, ppHiddenNoWindows = dzenColor "#444444"  "" . pad
	, ppUrgent	= dzenColor "" "red"
	, ppWsSep    = ""
	, ppSep      = "|"
	, ppLayout   = dzenColor "green" "" .
	(\ x -> case x of
		"Maximize Tall"			-> "[]="
		"Maximize Mirror Tall"		-> "TTT"
		"Maximize Full"			-> "<M>"
		"Maximize Grid"			-> "+++"
		"Maximize Spiral"			-> "(@)"
		"Maximize Accordion"		-> "Acc"
		"Maximize Tabbed Simplest"	-> "Tab"
		"Maximize Tabbed Bottom Simplest"	-> "TaB"
		"Maximize SimplestFloat"		-> "><>"
		"Maximize IM"			-> "IM "
		"Maximize Dishes 2 (1%6)"		-> "Dsh"
		_				-> pad x
	)
	, ppTitle    = (" " ++) . dzenColor "green" "" . dzenEscape 
	, ppOutput = hPutStrLn h
	}



-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    -- close focused window 
    , ((modm .|. shiftMask, xK_c     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    -- , ((modm , xK_b ), sendMessage ToggleStruts)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)
    ]
    ++
 
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- alt-[F1..F9], Switch to workspace N
    -- alt-shift-[F1..F9], Move client to workspace N
    [((m .|. mod1Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 

 
