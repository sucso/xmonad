import XMonad hiding (defaultConfig)
import XMonad.Core (screenRect, windowset)

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Promote
import XMonad.Actions.WithAll (killAll, sinkAll)
import qualified XMonad.Actions.FlexibleResize as Flex

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing (swallowEventHook)

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.Magnifier as Mag

import qualified XMonad.StackSet as W

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (mkKeymap, mkNamedKeymap)
import XMonad.Util.NamedActions (NamedAction, addName, addDescrKeys', subtitle, showKm, (^++^))
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Ungrab (unGrab)
import qualified XMonad.Util.Hacks as Hacks

import Data.Char (toLower, isSpace)
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

myLayouts = tiled ||| threeCol ||| monocle ||| spirals
  where
    tiled = renamed [Replace "Tile"]
      $ Tall nmaster delta ratio
      where
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2
    threeCol = renamed [Replace "Three Columns"]
      $ ThreeColMid nmaster delta ratio
      where
        nmaster = 1         -- Default number of windows in the master pane
        delta   = 3/100     -- Percent of screen to increment by when resizing panes
        ratio   = 1/2       -- Default proportion of screen occupied by master pane
    monocle = renamed [Replace "Monocle"]
      $ Full
    spirals = renamed [Replace "Spirals"]
      $ spiral ratio
      where
        ratio = 6/7

    -- TODO: look into "Dwindle" layout
    --       ( https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-Dwindle.html )

    -- TODO: look into "Grid Variants" layout
    --       ( https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-GridVariants.html )

    -- TODO: use CenteredIfSingle
    --       ( https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Layout-CenteredIfSingle.html )

myLayoutHook = avoidStruts
  $ smartBorders
  myLayouts

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
  ]

mySwallowClasses = [ "st-256color"
                   , "xterm"
                   ]

{- *******************************
              KEYBINDINGS
   ******************************* -}

myModMask = mod4Mask

myToggleXMobar = "dbus-send \
                 \ --session \
                 \ --dest=org.Xmobar.Control \
                 \ --type=method_call \
                 \ --print-reply '/org/Xmobar/Control' \
                 \ org.Xmobar.Control.SendSignal 'string:Toggle 0'"


showKeyBindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeyBindings keymap = addName "Show keybindings" $ do
  rect <- gets (screenRect . W.screenDetail . W.current . windowset)
  let width  = rect_width  rect `div` 2
      height = rect_height rect `div` 2
  font <- io fontMono
  handle <- spawnPipe $ "yad \
                        \ --text-info \
                        \ --fontname=" ++ font ++ " 18\
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
      [ ("M-b",   addName "Toggle hide/show bar"                  $ spawn myToggleXMobar) -- sendMessage ToggleStruts
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
      [ ("M-t", addName "Tile"          $ sendMessage $ JumpToLayout "Tile")
      , ("M-r", addName "Three Columns" $ sendMessage $ JumpToLayout "Three Columns")
      , ("M-o", addName "Monocle"       $ sendMessage $ JumpToLayout "Monocle")
      , ("M-s", addName "Spirals"       $ sendMessage $ JumpToLayout "Spirals")
      ]

      ^++^ subKeys "Control current layout and windows"
      [ ("M-[",       addName "Increase the number of master windows" $ sendMessage $ IncMasterN 1)
      , ("M-]",       addName "Decrease the number of master windows" $ sendMessage $ IncMasterN (-1))
      , ("M-j",       addName "Move focus down"                       $ windows W.focusDown)
      , ("M-k",       addName "Move focus up"                         $ windows W.focusUp)
      , ("M-m",       addName "Move focus to master"                  $ windows W.focusMaster)
      , ("M-S-j",     addName "Swap window down"                      $ windows W.swapDown)
      , ("M-S-k",     addName "Swap window up"                        $ windows W.swapUp)
      , ("M-S-m",     addName "Swap window with master"               $ windows W.swapMaster)
      , ("M-<Space>", addName "Promote window to master"              $ promote)
      , ("M-s",         addName "Sink focused window"                   $ withFocused $ windows . W.sink)
      , ("M-S-s",       addName "Sink all windows"                      $ sinkAll)
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
                MAIN
   ******************************* -}

myWorkspaces :: [String]
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp "xmobar" $ clickablePP myPrettyPrinter

myPrettyPrinter :: PP
myPrettyPrinter = def
  { ppRename = \w _ -> xmobarRaw w

  , ppCurrent = xmobarColor base0E "" . wrap
                ("<box type=Bottom width=2 mb=2 color=" ++ base0E ++ ">") "</box>"

    -- Visible but not current workspace
  , ppVisible = xmobarColor base0E ""

    -- Hidden workspace
  , ppHidden = xmobarColor base0A "" . wrap
               ("<box type=Top width=2 mt=2 color=" ++ base0A ++ ">") "</box>"

    -- Hidden workspaces (no windows)
  , ppHiddenNoWindows = xmobarColor base0A ""

    -- Title of active window
  , ppTitle = xmobarColor base0D "" . xmobarStrip . shorten 90

    -- Separator character
  , ppSep =  "<fc=" ++ base0F ++ "> | </fc>"

  , ppLayout = xmobarColor base0B ""

    -- Urgent workspace
  , ppUrgent = xmobarColor base0C "" . wrap "[" "]"

    -- Number of windows in current workspace
  , ppExtras  = [ gets $ Just . show . length . W.index . windowset ]

    -- order of things in xmobar
  , ppOrder  = \(workspaces:layout:titles:extras) -> [workspaces,layout]++extras++[titles]
  }

-- TODO: XMonad.Actions.Navigation2D ( https://www.cs.dal.ca/~nzeh/xmonad/Navigation2D.pdf )

main :: IO ()
main = do
  terminal <- myTerminal
  xmonad
    . ewmh
    . docks
    . withSB myStatusBar
    . addDescrKeys' ((mod4Mask .|. shiftMask, xK_F1), showKeyBindings) myKeyBindings
    $
    def
      { modMask = myModMask
      , terminal = terminal
      , handleEventHook = handleEventHook def
        <> Hacks.windowedFullscreenFixEventHook
        <> swallowEventHook (foldl1 (<||>) $ map (className =?) mySwallowClasses) (return True)
      , layoutHook = myLayoutHook
      , manageHook = myManageHook
      , mouseBindings = myMouseBindings
      , workspaces = myWorkspaces
      , normalBorderColor = base02
      , focusedBorderColor = base09
      , borderWidth = 3
      }
