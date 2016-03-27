
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (hPutStrLn)
import XMonad
    ( ChangeLayout(NextLayout), Dimension, KeyMask, Layout, ManageHook, Resize(Expand, Shrink)
    , IncMasterN(IncMasterN), X
    , XConfig(XConfig, borderWidth, keys, logHook, modMask), (.|.), (-->)
    , (=?), (<+>), composeAll, doIgnore, float, get, gets, kill, layoutHook
    , manageHook, refresh, resource, restart, screenRect, screenWorkspace, sendMessage
    , setLayout, spawn, terminal, tileWindow, windows, windowset, withFocused, whenJust
    , workspaces, xmonad )
import XMonad.Actions.CycleWS (shiftNextScreen, swapNextScreen)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.ManageDocks (ToggleStruts(ToggleStruts), avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers
    ( (-?>), composeOne, doFullFloat, isFullscreen, MaybeManageHook )
import XMonad.Hooks.DynamicLog
    ( dynamicLogWithPP, ppOutput, ppTitle, shorten, xmobarPP, xmobarColor )
import qualified XMonad.StackSet as W
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
    --display <- openDisplay ""
    --let defaultScreen = defaultScreen display
    --let defaultScreen = defaultScreenOfDisplay display
    --let screenSize = widthOfScreen defaultScreen

    xmobarProc <- spawnPipe "xmobar"

    xmonad def
        { modMask = myModMask
        , layoutHook = myLayout
        , keys = myKeys
        , terminal = "on-screen roxterm"
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmobarProc
            , ppTitle = xmobarColor "lightblue" "" . shorten 150
            }
        , borderWidth = myBorderWidth
        , manageHook = manageDocks
                   <+> manageHook def
                   <+> composeAll myManagementHooks
                   <+> composeOne myMaybeManagementHooks
        }

-- check out http://haskell.org/haskellwiki/Xmonad/Config_archive/Template_xmonad.hs_(darcs)
-- for the defaults

myBorderWidth :: Dimension
myBorderWidth = 5

-- Make the mod key be the windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- avoidStruts will make sure not avoid any sort of menu or status bar.
-- smartBorders will only use a border where necessary.
myLayout = avoidStruts $ smartBorders $ layoutHook def

-- Special actions to be performed on newly created windows matching
-- specific properties.
myManagementHooks :: [ManageHook]
myManagementHooks = [ resource =? "stalonetray" --> doIgnore
                    ]

-- Special actions to be performed on newly created windows matching
-- specific properties. Only returns action for first ManageHook that
-- returns a Just.
myMaybeManagementHooks :: [MaybeManageHook]
myMaybeManagementHooks = [ -- Make sure that full screened windows like
                           -- firefox's flash are actually shown as full
                           -- screen.
                           isFullscreen -?> doFullFloat
                         ]

-- Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
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
    , ((0, xK_Print), spawn "screenshot-to-clipboard")

    -- Use Ctrl-F12 to re-setup the keyboard.
    , ((controlMask, xK_F12), spawn "setup-keyboard")

    -- Use mod-s to make a window floating and resize it for a screencast
    , ((modm, xK_s), withFocused setupWindowForScreenCast)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    ]

-- Switch to the unfocused screen.  Does nothing if not exactly two
-- screens.
switchToUnfocusedScreen :: X ()
switchToUnfocusedScreen = do
        allWorkspaces <- gets windowset
        let visibleNonFocusedScreens = W.visible allWorkspaces
        -- TODO: This only works for two screens.  It would be nice if it
        -- worked for 3 or more screens.
        case visibleNonFocusedScreens of
            [screen] -> windows $ W.view $ W.tag $ W.workspace screen
            _ -> return ()

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
