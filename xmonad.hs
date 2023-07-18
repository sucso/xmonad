import XMonad hiding (defaultConfig)
import XMonad.Core (screenRect, windowset)

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Promote
import XMonad.Actions.WithAll (killAll, sinkAll)
import qualified XMonad.Actions.FlexibleResize as Flex

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog)
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WindowSwallowing (swallowEventHook)

import XMonad.Layout.BorderResize
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.Decoration
import XMonad.Layout.Dwindle
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Layout.MultiToggle as MT

import qualified XMonad.StackSet as W

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (mkKeymap, mkNamedKeymap)
import XMonad.Util.NamedActions (NamedAction, addName, addDescrKeys', subtitle, showKm, (^++^))
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Ungrab (unGrab)
import qualified XMonad.Util.Hacks as Hacks

import Codec.Binary.UTF8.String (encode)
import Data.Char (toLower, isSpace)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M

import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr)

import XResources

{- *******************************
        ENVIRONMENT VARIABLES
   ******************************* -}

-- Applications
myBrowser   = fromMaybe "librewolf"       <$> lookupEnv "BROWSER"
myTerminal  = fromMaybe (terminal def)    <$> lookupEnv "TERMINAL"
myLauncher  = fromMaybe "rofi -show drun" <$> lookupEnv "LAUNCHER"
myBookmarks = fromMaybe ""                <$> lookupEnv "LAUNCHER_BOOKMARKS"

-- Fonts
fontEmoji = fromMaybe "Noto Color Emoji" <$> lookupEnv "FONT_EMOJI"
fontMono  = fromMaybe "DejaVu Sans Mono" <$> lookupEnv "FONT_MONO"
fontSans  = fromMaybe "DejaVu Sans"      <$> lookupEnv "FONT_SANS"
fontSerif = fromMaybe "DejaVu Serif"     <$> lookupEnv "FONT_SERIF"

{- *******************************
               LAYOUTS
   ******************************* -}

myTheme :: Theme
myTheme = def { activeColor         = black
              , activeBorderColor   = orange
              , activeTextColor     = white

              , inactiveColor       = black
              , inactiveBorderColor = gray
              , inactiveTextColor   = gray

              , urgentColor         = red
              , urgentBorderColor   = yellow
              , urgentTextColor     = black

              , fontName            = "xft:monospace"
              , windowTitleIcons    = windowTitleIcons defaultThemeWithImageButtons
              , decoWidth           = 200
              , decoHeight          = 18
              }

myLayouts = tall ||| full ||| dwindle
  where
    tall = renamed [Replace "Tall"]
      $ centeredIfSingle (3/4) 1
      $ ResizableTall nmaster delta ratio []
      where
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2
    full = renamed [Replace "Full"] Full
    dwindle = renamed [Replace "Dwindle"]
      $ centeredIfSingle (3/4) 1
      $ Dwindle firstSplitDirection chirality ratio delta
      where
        firstSplitDirection = R
        chirality = CW
        ratio = 1.00 -- 6/7
        delta = 1.10

myFloatingLayout = renamed [Replace "Float"]
  $ imageButtonDeco shrinkText myTheme
  $ borderResize
  $ positionStoreFloat

myLayoutHook = avoidStruts
  . smartBorders
  . toggleLayouts myFloatingLayout
  . MT.mkToggle (MT.single REFLECTX)
  . MT.mkToggle (MT.single REFLECTY)
  $ myLayouts

{- *******************************
               WINDOWS
   ******************************* -}

-- NOTE: ManageHooks compose from right to left, like function composition:
--         executed_last <> executed_next <> ... <> executed_first;
-- NOTE: composeAll executes all matching rules (from the top of the list to the bottom);
--       composeOne runs only the first match.
myManageHook = composeAll
  [ isDialog                       --> doCenterFloat
  , className =? "confirm"         --> doFloat
  , className =? "file_progress"   --> doFloat
  , className =? "dialog"          --> doCenterFloat
  , className =? "download"        --> doFloat
  , className =? "error"           --> doFloat
  , className =? "notification"    --> doFloat
  , className =? "splash"          --> doFloat
  , className =? "Yad"             --> doCenterFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , manageDocks
  , fmap not willFloat --> insertPosition End Newer
  , positionStoreManageHook (Just myTheme)
  ]

mySwallowClasses = [ "st-256color"
                   , "xterm"
                   ]

{- *******************************
              KEYBINDINGS
   ******************************* -}

myModMask = mod4Mask

showKeyBindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeyBindings keymap = addName "Show keybindings" $ do
  rect <- gets (screenRect . W.screenDetail . W.current . windowset)
  let width  = rect_width  rect `div` 2
      height = rect_height rect `div` 2
  font <- io fontMono
  handle <- spawnPipe $ "yad \
                        \ --text-info \
                        \ --fontname=" ++ font ++ " 18 \
                        \ --fore=" ++ base05 ++ " \
                        \ --back=" ++ base00 ++ " \
                        \ --center \
                        \ --geometry=" ++ show width ++ "x" ++ show height ++ " \
                        \ --no-buttons \
                        \ --undecorated \
                        \ --title 'XMonad keybindings'"
  io $ hPutStr handle (unlines $ showKm keymap)
  io $ hClose handle
  return ()

myKeyBindings :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeyBindings c =
  let subKeys str ks = subtitle str : mkNamedKeymap c ks in
      subKeys "XMonad Core"
      [ ("M-b",   addName "Toggle hide/show bar"                  $ toggleStruts)
      , ("M-S-q", addName "Quit XMonad"                           $ io exitSuccess)
      , ("M-S-c", addName "Kill focused window"                   $ kill1)
      , ("M-S-a", addName "Kill all windows in current workspace" $ killAll)
      ]

      ^++^ subKeys "Launch applications"
      [ ("M-d",        addName "Spawn application launcher" $ spawn =<< io myLauncher)
      , ("M-e",        addName "Spawn Emacs client"         $ spawn "emacsclient -c -a \"\"")
      , ("M-f",        addName "Spawn bookmarks launcher"   $ spawn =<< io myBookmarks)
      , ("M-<Return>", addName "Spawn terminal"             $ spawn =<< io myTerminal)
      , ("<Print>",    addName "Spawn screenshot utility"   $ unGrab *> spawn "flameshot gui")
      ]

      ^++^ subKeys "Switch workspace"
      ([("M-" ++ show number,   addName ("Switch to workspace n°" ++ show number) $ windows $ W.greedyView $ myWorkspaces !! pred number) | number <- [1..9]]
      ++ [("M-p", addName "Toggle previous workspace" toggleWS)])

      ^++^ subKeys "Send windows to workspace"
      [("M-S-" ++ show number, addName ("Send window to workspace n°" ++ show number) $ windows $ W.shift $ myWorkspaces !! pred number) | number <- [1..9]]

      ^++^ subKeys "Layouts"
      [ ("M-t", addName "Tall"          $ sendMessage $ JumpToLayout "Tall")
      , ("M-o", addName "Full"          $ sendMessage $ JumpToLayout "Full")
      , ("M-r", addName "Dwindle"       $ sendMessage $ JumpToLayout "Dwindle")
      ]

      ^++^ subKeys "Control current layout and windows"
      [ ("M-[",         addName "Increase the number of master windows" $ sendMessage $ IncMasterN 1)
      , ("M-]",         addName "Decrease the number of master windows" $ sendMessage $ IncMasterN (-1))
      , ("M-j",         addName "Move focus down"                       $ windows W.focusDown)
      , ("M-k",         addName "Move focus up"                         $ windows W.focusUp)
      , ("M-m",         addName "Move focus to master"                  $ windows W.focusMaster)
      , ("M-S-j",       addName "Swap window down"                      $ windows W.swapDown)
      , ("M-S-k",       addName "Swap window up"                        $ windows W.swapUp)
      , ("M-<Space>",   addName "Swap window with master"               $ windows W.swapMaster)
      -- , ("M-<Space>", addName "Promote window to master"              $ promote)
      , ("M-M1-h",      addName "Shrink horizontally"                   $ sendMessage Shrink)
      , ("M-M1-j",      addName "Shrink vertically"                     $ sendMessage MirrorShrink)
      , ("M-M1-k",      addName "Expand vertically"                     $ sendMessage MirrorExpand)
      , ("M-M1-l",      addName "Expand horizontally"                   $ sendMessage Expand)
      , ("M-s",         addName "Sink focused window"                   $ withFocused $ windows . W.sink)
      , ("M-S-s",       addName "Sink all windows"                      $ sinkAll)
      , ("M-x",         addName "Reflect layout horizontally"           $ sendMessage $ MT.Toggle REFLECTX)
      , ("M-z",         addName "Reflect layout vertically"             $ sendMessage $ MT.Toggle REFLECTY)
      ]

      ^++^ subKeys "Music Player Daemon Control (MPC)"
      [ ("M-<F1>",  addName "Clear playlist"   $ spawn "mpc clear")
      , ("M-<F2>",  addName "Decrease volume"  $ spawn "mpc volume -2")
      , ("M-<F3>",  addName "Increase volume"  $ spawn "mpc volume +2")
      --("M-<F4>"   addName "" $ )
      , ("M-<F5>",  addName "Previous song"    $ spawn "mpc prev")
      , ("M-<F6>",  addName "Toggle playback"  $ spawn "mpc toggle")
      , ("M-<F7>",  addName "Next song"        $ spawn "mpc next")
      , ("M-<F8>",  addName "Toggle single"    $ spawn "mpc single")
      , ("M-<F9>",  addName "Shuffle playlist" $ spawn "mpc shuffle")
      , ("M-<F10>", addName "Toggle repeat"    $ spawn "mpc repeat")
      , ("M-<F11>", addName "Seek backward"    $ spawn "mpc seek -5")
      , ("M-<F12>", addName "Seek forward"     $ spawn "mpc seek +5")
      ]
      where
        togglePolybar = spawn "polybar-msg cmd toggle &"
        toggleStruts = togglePolybar >> sendMessage ToggleStruts

-- NOTE: you may also bind events to the mouse scroll wheel (button4 and button5)
myMouseBindings (XConfig { XMonad.modMask = myModMask}) = M.fromList
    -- Button1: Set the window to floating mode and move by dragging
    -- Button2: Sink window and raise it to the top of the stack
    -- Button3: Set the window to floating mode and resize by dragging
    [ ((myModMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((myModMask, button2), \w -> focus w >> withFocused (windows . W.sink) >> windows W.shiftMaster)
    , ((myModMask, button3), \w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster)
    ]
{- *******************************
             STATUS BAR
   ******************************* -}

-- from XMonad.Hooks.TaffybarPagerHints "setCurrentLayoutProp" function
setXProp :: String -> String -> X ()
setXProp prop val = withDisplay $ \dpy -> do
  root <- asks theRoot
  atom <- getAtom prop
  codec <- getAtom "UTF8_STRING"
  let val' = map fromIntegral (encode val)
  io $ changeProperty8 dpy root atom codec propModeReplace val'

myStatusBarLogHook :: X ()
myStatusBarLogHook = withWindowSet $ \windowSet -> do
  let property = "_CURRENT_LAYOUT"

  -- https://stackoverflow.com/a/62075879
  windowCount <- length . W.index . windowset <$> get
  -- windowCount <- length . W.integrate' . W.stack . W.workspace . W.current . windowset <$> get

  let layoutName = description . W.layout . W.workspace . W.current $ windowSet
  setXProp property (layoutName ++ " (" ++ show windowCount ++ ")")

{- *******************************
                MAIN
   ******************************* -}

myWorkspaces :: [String]
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

-- server mode commands
myCommands :: X [(String, X ())]
myCommands = do
  return [ ("toggle-layouts", sendMessage ToggleLayout) ]

main :: IO ()
main = do
  terminal <- myTerminal
  xmonad
    . ewmh
    . docks
    . addDescrKeys' ((mod4Mask .|. shiftMask, xK_F1), showKeyBindings) myKeyBindings
    $
    def
      { modMask = myModMask
      , terminal = terminal
      , handleEventHook = handleEventHook def
        <> positionStoreEventHook
        <> serverModeEventHookCmd' myCommands
        <> Hacks.windowedFullscreenFixEventHook
        <> swallowEventHook (foldl1 (<||>) $ map (className =?) mySwallowClasses) (return True)
      , layoutHook = myLayoutHook
      , logHook = myStatusBarLogHook
      , manageHook = myManageHook
      , mouseBindings = myMouseBindings
      , workspaces = myWorkspaces
      , normalBorderColor = inactiveBorderColor myTheme
      , focusedBorderColor = activeBorderColor myTheme
      , borderWidth = 3
      }
