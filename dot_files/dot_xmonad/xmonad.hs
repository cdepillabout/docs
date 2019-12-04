{-# OPTIONS_GHC -Wall #-}

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
       (xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)
import Graphics.X11.Xlib
import System.Exit (ExitCode(ExitSuccess), exitWith)
import XMonad
       (ChangeLayout(NextLayout), Dimension, KeyMask, Layout,
        Resize(Expand, Shrink), IncMasterN(IncMasterN), ScreenId,
        ScreenDetail, WindowSet, WorkspaceId, X,
        XConfig(XConfig, borderWidth, keys, logHook, modMask), (.|.),
        float, get, kill, layoutHook, manageHook, refresh, restart,
        screenRect, screenWorkspace, sendMessage, setLayout, spawn,
        terminal, tileWindow, windows, windowset, withFocused,
        withWindowSet, whenJust, workspaces, xmonad)
import XMonad.Actions.CycleWS (shiftNextScreen, swapNextScreen)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.ManageDocks
       (AvoidStruts, ToggleStruts(ToggleStruts), avoidStruts, docks,
        manageDocks)
import XMonad.Layout (Choose, Full, Mirror, Tall)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.WorkspaceHistory
       (workspaceHistory, workspaceHistoryHook)
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  configWithXMobar <- xmobar . ewmh $ docks myXMonadConfig
  xmonad configWithXMobar

myXMonadConfig
  :: XConfig
      (ModifiedLayout
        AvoidStruts
        (ModifiedLayout
          SmartBorder
            (Choose
              Tall
              (Choose (Mirror Tall) Full)
            )
        )
      )
myXMonadConfig =
  def
    { borderWidth = myBorderWidth
    , keys = myKeys
    , layoutHook = myLayout
    , logHook = myLogHook
    , manageHook = manageDocks
    , modMask = myModMask
    , terminal = "termonad"
    }

myLogHook :: X ()
myLogHook = workspaceHistoryHook

myBorderWidth :: Dimension
myBorderWidth = 5

-- Make the mod key be the windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- avoidStruts will make sure not avoid any sort of menu or status bar.
-- smartBorders will only use a border where necessary.
myLayout
  :: ModifiedLayout
       AvoidStruts
       (ModifiedLayout
         SmartBorder
         (Choose Tall (Choose (Mirror Tall) Full)))
       Window
myLayout = avoidStruts . smartBorders $ layoutHook def

-- Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ terminal conf)
    -- launch dmenu
    -- , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modm,               xK_p     ), spawn "dmenu_run")
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    -- , ((modm,               xK_Tab   ), windows W.focusDown)
    -- , ((mod1Mask,           xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp)
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
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
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), liftIO $ exitWith ExitSuccess)
    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    -- alt-[F1..F9], Switch to workspace N
    -- alt-shift-[F1..F9], Move client to workspace N
    -- XXX: This is almost completely unlike fluxbox, so I will
    -- not use it for now.
    [((m .|. mod1Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    -- My settings
    [
    -- Swap the current screen with the next screen.
      ((modm,               xK_o), swapNextScreen)
    -- Swap the current screen with the previous screen.
    , ((modm .|. shiftMask, xK_o), shiftNextScreen)

    -- Spawn xscreensaver
    , ((modm .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

    -- Switch to the unfocused screen.  Does nothing if not exactly two
    -- screens.
    , ((modm,               xK_Tab), switchToUnfocusedScreen)
    , ((mod1Mask,           xK_Tab), switchToUnfocusedScreen)

    -- Use printscreen key for taking screenshot and copying it to the
    -- clipboard.
    , ((noModMask, xK_Print), spawn "screenshot-to-clipboard")

    -- Use Ctrl-F12 to re-setup the keyboard.
    , ((controlMask, xK_F12), spawn "setup-keyboard")

    -- Use mod-s to make a window floating and resize it for a screencast
    , ((modm, xK_s), withFocused setupWindowForScreenCast)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- open graphical emacs
    , ((modm, xK_i), spawn "gemacs")

    -- open firefox
    , ((modm, xK_f), spawn "firefox")

    -- open chromium
    , ((modm, xK_g), spawn "chromium")

    -- control monitor brightness
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-")
    , ((noModMask, xF86XK_MonBrightnessUp), spawn "brightnessctl set 10%+")
    ]

-- Switch to the previously focused workspace that is visible on a Xinerama
-- screen.
switchToUnfocusedScreen :: X ()
switchToUnfocusedScreen = do
    withWindowSet $ \windowSet -> do
        history <- workspaceHistory
        case filter (isWorkspaceVisible windowSet) history of
            -- We ignore the very first screen because it is the currently
            -- focused screen.
            (_:workspaceId:_) -> windows $ W.view workspaceId
            _ -> pure ()

setupWindowForScreenCast :: Window -> X ()
setupWindowForScreenCast window = do
    xstate <- get
    let windowSet = windowset xstate
        currentScreen = W.current windowSet
        screenDetail = W.screenDetail currentScreen
        screenRectangle = screenRect screenDetail
        screenX = rect_x screenRectangle
    tileWindow window (Rectangle (100 + screenX) 100 1280 720)
    float window


-------------
-- HELPERS --
-------------

type Screen' = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- | Return whether or not 'WorkspaceId' is a visible 'Workspace' in
-- 'WindowSet'.
isWorkspaceVisible :: WindowSet -> WorkspaceId -> Bool
isWorkspaceVisible windowSet workspaceId =
    isWorkspaceOnScreen workspaceId (W.current windowSet) ||
        any (isWorkspaceOnScreen workspaceId) (W.visible windowSet)

-- | Get a 'WorkspaceId' from a 'Screen'.
workspaceIdFromScreen :: Screen' -> WorkspaceId
workspaceIdFromScreen = W.tag . W.workspace

-- | Return 'True' if a 'WorkspaceId' is on a 'Screen'.
isWorkspaceOnScreen :: WorkspaceId -> Screen' -> Bool
isWorkspaceOnScreen workspaceId screen =
    workspaceIdFromScreen screen == workspaceId
