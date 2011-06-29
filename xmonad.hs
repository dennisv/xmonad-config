import XMonad

import System.IO                   (hPutStrLn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run             (spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace  (onWorkspace)
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig        (additionalKeysP)
import XMonad.Hooks.ManageHelpers  (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import qualified XMonad.StackSet as W

import Data.Ratio ((%))

main :: IO ()
main = do
    myStatusBarPipe <- spawnPipe myStatusBar
    spawn "~/bin/dzconky"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces         = myWorkspaces
        , logHook            = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn myStatusBarPipe }
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
        } `additionalKeysP` myKeys

myStatusBar :: String
myStatusBar = "dzen2 " ++ myStatusBarArgs

myStatusBarArgs :: String
myStatusBarArgs = "-fn 'Verdana-8' -bg '#1a1a1a' -fg '#aaaaaa' -h 20 -w 840 -ta l "

myTerminal :: String
myTerminal = "urxvtc"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = "#261E20"

myFocusedBorderColor :: String
myFocusedBorderColor = "#FF1F66"

myWorkspaces :: [WorkspaceId]
myWorkspaces = 
    [ withIcon "root.xbm" "root"
    , withIcon "fox.xbm" "web"
    , withIcon "code.xbm" "code"
    , withIcon "mail.xbm" "mail"
    , withIcon "mouse.xbm" "gfx"
    , withIcon "cpu.xbm" "im"
    ]
    where
        withIcon icon title = "^i(" ++ iconsPath ++ icon ++ ") " ++ title

myDzenPP :: PP
myDzenPP = dzenPP
    { ppHidden          = dzenColor "#CCCCCC" "#444444" . hideNSP
    , ppCurrent         = dzenColor "#FFFFFF" "#FF1F66" . pad'
    , ppUrgent          = dzenColor "#800000" "#FF91CF" . dzenStrip
    , ppHiddenNoWindows = dzenColor "#444444" "" . pad'
    , ppTitle           = dzenColor "#909090" "" . pad'
    , ppLayout          = dzenColor "#909090" "" . pad' . \s ->
        case s of
            "ResizableTall"        -> "^fg(#FF1F66)^i(" ++ iconsPath ++ "tall.xbm)^fg()"
            "Mirror ResizableTall" -> "^fg(#FF1F66)^i(" ++ iconsPath ++ "mtall.xbm)^fg()"
            "Full"                 -> "^fg(#FF1F66)^i(" ++ iconsPath ++ "full.xbm)^fg()"
            "IM ReflectX IM Full"  -> "^fg(#FF1F66)^i(" ++ iconsPath ++ "mouse.xbm)^fg()"
            "IM ResizableTall"     -> "^fg(#FF1F66)^i(" ++ iconsPath ++ "cpu.xbm)^fg()"
            _                      -> pad s
    }

iconsPath :: String
iconsPath = "/home/dennis/.xmonad/icons/"

hideNSP :: WorkspaceId -> String
hideNSP ws = if ws /= "NSP" then pad' ws else ""

pad' :: String -> String
pad' = wrap "  " "  "

myLayoutHook = avoidStruts $ onWorkspace (myWorkspaces !! 4) gimpLayout $ onWorkspace (myWorkspaces !! 5) chatLayout $ myLayouts
    where
        myLayouts  = tiled ||| Mirror tiled ||| full
        chatLayout = withIM (1%5) chatProp myLayouts
        chatProp   = Role "buddy_list"

        gimpLayout = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full

        tiled      = smartBorders (ResizableTall nmaster delta ratio [])
        full       = noBorders Full

        nmaster    = 1
        delta      = 3/100
        ratio      = toRational (2/(1 + sqrt 5 :: Double))

-- From pbrisbin's config
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks                                      ]
    , [ manageHook defaultConfig                         ]
    , [ isDialog     --> doCenterFloat                   ]
    , [ isFullscreen --> doF W.focusDown <+> doFullFloat ]
    ]

myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ]
    where myActions = [ ("Xmessage"  , doCenterFloat               )
                      , ("Chromium"  , doShift (myWorkspaces !! 1) )
                      , ("Firefox"   , doShift (myWorkspaces !! 1) )
                      , ("gimp"      , doShift (myWorkspaces !! 4) )
                      , ("pidgin"    , doShift (myWorkspaces !! 5) )
                      ]

matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

name :: Query String
name = stringProperty "WM_NAME"

role :: Query String
role = stringProperty "WM_ROLE"

cleanStart :: MonadIO m => m ()
cleanStart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && "
                  ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                  ++ "xmonad --recompile && xmonad --restart"

launchDmenu :: MonadIO m => m ()
launchDmenu = spawn "exe=`dmenu_path | ~/bin/dmenu` && eval \"exec $exe\""

myKeys :: [(String, X())]
myKeys = [ ( "M-p"                    , launchDmenu                         ) -- dmenu app launcher
         , ( "M-l"                    , spawn "slock"                       ) -- lock screen
         , ( "<XF86AudioMute>"        , spawn "amixer -q set Master toggle" ) -- toggle mute
         , ( "<XF86AudioLowerVolume>" , spawn "amixer -q set Master 2%-"    ) -- volume down
         , ( "<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 2%+"    ) -- volume up
         , ( "M-q"                    , cleanStart                          ) -- restart xmonad
         ]
