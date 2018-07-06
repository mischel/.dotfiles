

--- * import ---------------------------------------------------------------------------------------------------------

import qualified Data.Map                    as M

import           XMonad                      as X
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowGo    (raiseMaybe)
import           XMonad.Config.Xfce         (xfceConfig)
import           XMonad.Hooks.EwmhDesktops  (fullscreenEventHook, ewmhDesktopsLogHookCustom)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks   (avoidStruts)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName     (setWMName)
import           XMonad.Layout.Circle
-- import           XMonad.Layout.Fullscreen   (fullscreenFull, fullscreenEventHook, fullscreenManageHook)
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run             (spawnPipe, hPutStrLn)
import           XMonad.Util.EZConfig        (additionalKeys, removeKeys)


--- * main -----------------------------------------------------------------------------------------------------------



main :: IO ()
main = do
  myDzen <-spawnPipe $ "sleep 6; dzen2 " ++ myDzenFlags
  -- xfceConfig = xfceConfig $ desktopConfig $ docs ewmh
  xmonad $ xfceConfig
    { normalBorderColor  = "#333333"
    , focusedBorderColor = "#4c7899"
    , terminal           = myTerminal

    , layoutHook         = myLayoutHook
    -- manage windows upon creation
    , manageHook         = myManageHook <+> namedScratchpadManageHook myNamedScratchpads <+> manageHook xfceConfig
    -- handle X and EWMH events
    , handleEventHook    = fullscreenEventHook <+> handleEventHook xfceConfig
    -- , handleEventHook    = handleEventHook xfceConfig

    , workspaces         = myWorkspaces
    -- , modMask            = xfceConfig modMask
    -- , keys               = \layout -> addKeys xfceConfig <+> keys xfceConfig layout
    -- , mouseBindings      = xfceConfig mouseBindings
    , borderWidth        = 1

    -- inform about changes in the window stack
    , logHook            = myDzenLogHook myDzen <+> myXfceLogHook
    , startupHook        = myStartupHook
    
    -- , focusFollowsMouse  = xfceConfig focusFollowsMouse -- True
    -- , clickJustFocuses   = xfceConfig clickJustFocuses  -- True

    -- , clientMask         :: !EventMask           -- ^ The client events that xmonad is interested in
    -- , rootMask           :: !EventMask           -- ^ The root events that xmonad is interested in
    -- , handleExtraArgs    :: !([String] -> XConfig Layout -> IO (XConfig Layout))
    }
    `additionalKeys`
      addKeys xfceConfig

-- fullscreen support (Firefox & Co)
-- https://github.com/binarin/rc/blob/58cc3b28045eecc41cdc3b4e2051b38a70117717/.xmonad/xmonad.hs#L251
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- getAtom "_NET_SUPPORTED"
  c <- getAtom "ATOM"
  supp <- mapM getAtom 
    ["_NET_WM_STATE_HIDDEN"
    ,"_NET_WM_STATE_FULLSCREEN" -- XXX Copy-pasted to add this line
    ,"_NET_NUMBER_OF_DESKTOPS"
    ,"_NET_CLIENT_LIST"
    ,"_NET_CLIENT_LIST_STACKING"
    ,"_NET_CURRENT_DESKTOP"
    ,"_NET_DESKTOP_NAMES"
    ,"_NET_ACTIVE_WINDOW"
    ,"_NET_WM_DESKTOP"
    ,"_NET_WM_STRUT" ]
  io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)
  setWMName "xmonad"

myTerminal :: String
myTerminal = "st-256color"

role = stringProperty "WM_WINDOW_ROLE"
isGMusic  = resource =? "play.google.com__music_listen"

doCentralFloat = doRectFloat $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

--- * workspace layouts ----------------------------------------------------------------------------------------------
-- @avoidStruts@  ... respect docker
-- @smartBorders@ ... remove borders for single/fullscreen windows

myWorkspaces :: [String]
myWorkspaces = ["α:misc","β:web","γ:dev","δ:im","ε:music","ζ:mail","η:fs","θ:foo","ι:bar"]

basicLayout :: ResizableTall a
basicLayout = ResizableTall nmaster delta ratio [] where
  nmaster = 1
  ratio   = toRational (2/(1+sqrt 5::Double))
  delta   = 3/100

tallLayout       = avoidStruts basicLayout
wideLayout       = avoidStruts $ Mirror basicLayout
singleLayout     = avoidStruts $ noBorders Full
fullscreenLayout = tallLayout ||| noBorders Full
pidginLayout     = avoidStruts $ reflectHoriz $ withIM (1/6) (Role "buddy_list") Grid

myLayoutHook = smartBorders $ im (fullscreen normal) where
  normal      = tallLayout ||| wideLayout ||| singleLayout ||| Circle
  fullscreen  = onWorkspace "η:fs" fullscreenLayout
  im          = onWorkspace "δ:im" pidginLayout


--- * startup --------------------------------------------------------------------------------------------------------

myStartupHook = do 
  startupHook xfceConfig 
  setWMName "LG3d" -- for JAVA GUI
  spawn "compton -CcG -o 0.3 --inactive-dim 0.3 --mark-wmwin-focused"
  spawn "xinput --set-prop 9 'libinput Accel Speed' 0.9"
  spawn "setxkbmap -option '' -option 'caps:super' && xcape -e 'Caps_Lock=Escape'"
  addEWMHFullscreen
  where
  addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
      sup <- (concat . (\dM -> case dM of {Just a -> [a]; Nothing -> []})) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
      if (fromIntegral x `notElem` sup)
        then changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]
        else pure ()
  addEWMHFullscreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

--- * manage hooks ---------------------------------------------------------------------------------------------------

myManageHook = 
  composeOne
    [ isDialog     -?> doCentralFloat -- doIgnore
    , isFullscreen -?> doFullFloat
    , transience ]
  <+>
  composeAll
    [ resource  =? "xfrun4"               --> doRectFloat (W.RationalRect 0 0 (2/3) (1/6))
    -- , className =? "Firefox"              --> doShift "β:web"
    , className =? "Eclipse"              --> doShift "γ:dev"
    , className =? "Pidgin"               --> doShift "δ:im"
    , className =? "Rhythmbox"            --> doShift "ε:music"
    , className =? "Thunderbird"          --> doShift "ζ:mail"
    , className =? "Xfce4-notifyd"        --> doF W.focusDown
    , className =? "feh"                  --> doFullFloat
    , resource  =? "desktop_window"       --> doIgnore ]

myNamedScratchpads = [ htop, gplay ] where
  htop  = inTerm "htop"
  -- MS: for some reason doFullFloat did not work with (L.isPrefixOf "Goole Play" <$> title)
  gplay = asApp  "gplay" "http://play.google.com/music/listen" isGMusic

  inTerm key       = NS key ("xfce4-terminal" ++ " -e "++ key ++ " -T " ++ key) (title =? key) doCentralFloat
  asApp key url qu = NS key ("chromium --temp-profile --incognito --app="++url) qu doFullFloat


--- * log hooks ------------------------------------------------------------------------------------------------------

myXfceLogHook = ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace

myDzenLogHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP . dzenLogHook

myDzenFlags :: String
myDzenFlags = "-fn "++ font ++ " -x 150 -w 800 -ta l -bg '#292929'"
  where font = "'-*-fixed-medium-*-*--12-*-*-*-*-*-iso88591-1'"

dzenLogHook handle = def
  { ppOutput          = hPutStrLn handle
  , ppCurrent         = dzenColor "#333333" "#4c7899" . pad
  , ppVisible         = dzenColor "red" "" . pad
  , ppHidden          = dzenColor "" "" . pad
  , ppUrgent          = dzenColor "red" "yellow"
  , ppTitle           = dzenColor "" "" . dzenEscape . pad
  , ppHiddenNoWindows = const ""
  , ppWsSep           = "|"
  , ppSep             = " || "
  , ppLayout          = dzenColor "" "" .
                (\ x -> case x of
                          "ResizableTall"        -> " []= "
                          "Mirror ResizableTall" -> " =;= "
                          "Full"                 -> " [ ] "
                          "Circle"               -> " (=) "
                          _                      -> pad x
                )
  }


--- * eys -----------------------------------------------------------------------------------------------------------

xdotool k = spawn ("xdotool key " ++ k)

addKeys c@(XConfig {X.modMask = modm}) =
  -- misc
  [ ((modm .|. shiftMask, xK_l), spawn "xflock4")
  -- , ((modm,               xK_p), spawn "$HOME/.local/bin/dmenu_run") 
  
  -- scratch
  , ((modm,               xK_s ),    submap . M.fromList $
      [ ((0, xK_h), namedScratchpadAction myNamedScratchpads "htop")
      , ((0, xK_m), namedScratchpadAction myNamedScratchpads "gplay")
      -- , ((0, xK_m), namedScratchpadAction myNamedScratchpads "mpsyt")
      -- , ((0, xK_l), namedScratchpadAction myNamedScratchpads "ghci")
      ])

  -- cycle
  , ((modm,               xK_Left),  moveTo Prev NonEmptyWS)
  , ((modm,               xK_Right), moveTo Next NonEmptyWS)
  , ((modm .|. shiftMask, xK_Left),  shiftToPrev)
  , ((modm .|. shiftMask, xK_Right), shiftToNext)
  , ((modm,               xK_Up),    nextScreen)
  , ((modm,               xK_Down),  prevScreen)
  , ((modm .|. shiftMask, xK_Up),    shiftNextScreen)
  , ((modm .|. shiftMask, xK_Down),  shiftPrevScreen)
  , ((modm,               xK_z),     toggleWS)
  --, ((modm,               xK_Down),  nextWS)
  --, ((modm,               xK_Up),    prevWS)

  -- raise
  , ((modm,               xK_f),     raiseFirefox)
  , ((modm,               xK_i),     raiseThunderbird)
  , ((modm,               xK_t),     raiseTerminal)

  -- vim movement
  , ((mod4Mask,           xK_h),     xdotool "Left")
  , ((mod4Mask,           xK_j),     xdotool "Down")
  , ((mod4Mask,           xK_k),     xdotool "Up")
  , ((mod4Mask,           xK_l),     xdotool "Right")

  -- grid
  , ((modm,               xK_g),     goToSelected   $ gsconfig myColorizer)
  , ((modm,               xK_b),     bringSelected  $ gsconfig myColorizer)

  -- misc
  -- , ((modm             , xK_q)      , spawn "killall xmonad && xmonad --recompile && xmonad --restart")
  , ((modm .|. shiftMask, xK_q),     spawn "cd ~/.xmonad ; nix-shell --pure --command 'ghc --make xmonad.hs -i -ilib -fforce-recomp -v0 -o xmonad-x86_64-linux && xmonad --restart'")
  ]
  ++
  [ ((m .|. mod1Mask, k), windows $ f i)
  | (i, k) <- zip myWorkspaces [xK_1 ..]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]


-- help = unlines ["The default modifier key is 'alt'. Default keybindings:",
--     "",
--     "-- launching and killing programs",
--     "mod-Shift-Enter  Launch xterminal",
--     "mod-p            Launch dmenu",
--     "mod-Shift-p      Launch gmrun",
--     "mod-Shift-c      Close/kill the focused window",
--     "mod-Space        Rotate through the available layout algorithms",
--     "mod-Shift-Space  Reset the layouts on the current workSpace to default",
--     "mod-n            Resize/refresh viewed windows to the correct size",
--     "",
--     "-- move focus up or down the window stack",
--     "mod-Tab        Move focus to the next window",
--     "mod-Shift-Tab  Move focus to the previous window",
--     "mod-j          Move focus to the next window",
--     "mod-k          Move focus to the previous window",
--     "mod-m          Move focus to the master window",
--     "",
--     "-- modifying the window order",
--     "mod-Return   Swap the focused window and the master window",
--     "mod-Shift-j  Swap the focused window with the next window",
--     "mod-Shift-k  Swap the focused window with the previous window",
--     "",
--     "-- resizing the master/slave ratio",
--     "mod-h  Shrink the master area",
--     "mod-l  Expand the master area",
--     "",
--     "-- floating layer support",
--     "mod-t  Push window back into tiling; unfloat and re-tile it",
--     "",
--     "-- increase or decrease number of windows in the master area",
--     "mod-comma  (mod-,)   Increment the number of windows in the master area",
--     "mod-period (mod-.)   Deincrement the number of windows in the master area",
--     "",
--     "-- quit, or restart",
--     "mod-Shift-q  Quit xmonad",
--     "mod-q        Restart xmonad",
--     "",
--     "-- Workspaces & screens",
--     "mod-[1..9]         Switch to workSpace N",
--     "mod-Shift-[1..9]   Move client to workspace N",
--     "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
--     "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
--     "",
--     "-- Mouse bindings: default actions bound to mouse events",
--     "mod-button1  Set the window to floating mode and move by dragging",
--     "mod-button2  Raise the window to the top of the stack",
--     "mod-button3  Set the window to floating mode and resize by dragging"]--

--- * mouse ----------------------------------------------------------------------------------------------------------
-- myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
--     -- mod-button1, Set the window to floating mode and move by dragging
--     [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
--                                        >> windows W.shiftMaster))
 
--     -- mod-button2, Raise the window to the top of the stack
--     , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
--     -- mod-button3, Set the window to floating mode and resize by dragging
--     , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
--                                        >> windows W.shiftMaster))
 
--     -- you may also bind events to the mouse scroll wheel (button4 and button5)
--     ]

--- **   raise -------------------------------------------------------------------------------------------------------

-- raiseChromium, :: X ()
-- raiseChromium = raiseMaybe (spawn "chromium --disk-cache-dir=\"/tmpfs\"") (className =? "Chromium" <&&> (not <$> isGMusic))
raiseChromium = raiseMaybe (spawn "chromium") (className =? "Chromium" <&&> (not <$> isGMusic))

raiseFirefox = raiseMaybe (spawn "firefox") (className =? "Firefox" <&&> (not <$> isGMusic))

raiseThunderbird :: X ()
raiseThunderbird = raiseMaybe (spawn "thunderbird") (className =? "Thunderbird")

raiseTerminal :: X ()
raiseTerminal = raiseMaybe (spawn "/home/c7031025/.local/bin/st") (className =? "Terminal")


--- ** grid ----------------------------------------------------------------------------------------------------------

gsconfig colorizer = (buildDefaultGSConfig colorizer)
  { gs_cellheight  = 30
  , gs_font        = appFontXft
  , gs_cellpadding = 5 }

myColorizer =
  colorRangeFromClassName
    (0xFF,0x40,0x00) -- lowest inactive bg
    (0x33,0x33,0x33) -- highest inactive bg
    (0x0D,0x17,0x1A) -- active bg
    minBound         -- inactive fg
    maxBound         -- active fg

appFontXft :: String
appFontXft =
  "'xft\
  \:Sans\
  \:pixelsize=10\
  \:weight=regular\
  \:width=semicondensed\
  \:dpi=96\
  \:hinting=true\
  \:hintstyle=hintslight\
  \:antialias=true\
  \:rgba=rgb\
  \:lcdfilter=lcdlight\
  \'"

