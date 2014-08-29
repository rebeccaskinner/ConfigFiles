import XMonad
import XMonad.Operations
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Util.CustomKeys
import XMonad.StackSet as W
import Data.List (find)
import System.IO
import Control.Monad

-- general definitions
terminalEmulator = "konsole"

-- define layouts
rzTall     = ResizableTall 1 (3/100) (1/2) []
fullLayout = named "Full" Full
tallLayout = named "Tall" rzTall
mirrorTall = named "Mirror" $ reflectHoriz rzTall
wideLayout = named "Wide" $ Mirror rzTall

myLayout = fullLayout ||| tallLayout ||| mirrorTall ||| wideLayout

curLayout :: X String
curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current

vimDir k
    | k == xK_h = L
    | k == xK_j = D
    | k == xK_k = U
    | k == xK_l = R

vimResizeMap k
    | k == xK_h = sendMessage $ Shrink
    | k == xK_j = sendMessage $ MirrorShrink
    | k == xK_k = sendMessage $ MirrorExpand
    | k == xK_l = sendMessage $ Expand

vimResizeMapMirror k
    | k == xK_h = sendMessage $ Expand
    | k == xK_j = sendMessage $ MirrorShrink
    | k == xK_k = sendMessage $ MirrorExpand
    | k == xK_l = sendMessage $ Shrink

vimGetResizeMap k = do
    str <- curLayout
    case str of
        "Mirror"  -> vimResizeMapMirror k
        otherwise -> vimResizeMap k
    return ()

vimKeys = [xK_h, xK_j, xK_k, xK_l]

deleteList _ = [(i,j)|i<-[mod4Mask,customShiftMask,customCtrlMask],j<-vimKeys]

keysList l =
       [((customShiftMask,x),sendMessage $ Swap (vimDir x))|x<-vimKeys]
    ++ [((customCtrlMask ,x),vimGetResizeMap x) | x <- vimKeys]
    ++ [((mod4Mask       ,x),sendMessage $ Go (vimDir x)) | x <- vimKeys]
    ++ [((mod4Mask       , xK_Up), spawn "xrandr --output DP-1 --rotate left --left-of DP-2"),
        ((mod4Mask       , xK_Down), spawn "xrandr --output DP-1 --rotate normal --left-of DP-2"),
        ((mod4Mask       , xK_x), spawn "xscreensaver-command --lock")]

customShiftMask = mod4Mask .|. shiftMask
customCtrlMask  = mod4Mask .|. controlMask

main = do
        xmproc <- spawnPipe "xmobar"
        xmonad $ defaultConfig
            { manageHook = manageDocks <+> manageHook defaultConfig
            , layoutHook = windowNavigation $ avoidStruts myLayout
            , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "#D2B6FA" "" . shorten 50
                }
            , modMask = mod4Mask
            , keys = customKeys deleteList keysList
            , borderWidth = 2
            , terminal = terminalEmulator
            , normalBorderColor = "#333333"
            , focusedBorderColor = "#FFAA00"
            }
