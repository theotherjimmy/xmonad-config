import qualified Data.Map as M
import Data.List (sort)
import XMonad hiding ((|||))
import XMonad.Prompt
import XMonad.StackSet as W
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators ((|||),JumpToLayout(..))
import XMonad.Util.Loggers

gsConfigWS :: GSConfig WorkspaceId
gsConfigWS = defaultGSConfig
gsConfigWin :: GSConfig Window
gsConfigWin = defaultGSConfig
gsConfigLay :: GSConfig [Char]
gsConfigLay = defaultGSConfig

layouts = ["Tall", "Wide", "Full", "Grid", "3 by 2", "Three Column"]

myLayouts = noBorders Full ||| tiled ||| (renamed [Replace "Wide"] $ Mirror tiled)
            ||| Grid
            ||| (renamed [Replace "3 by 2"] $ GridRatio (3/2))
            ||| (renamed [Replace "Three Column"] $ ThreeCol 1 (3/100) (1/3))
  where
    tiled = Tall 1 (3/100) (1/2)

gridselectLayout = gridselect gsConfigLay (zip layouts layouts)
                   >>= flip whenJust (sendMessage . JumpToLayout)

myGridselectWorkspace conf viewFn = withWindowSet $ \ws -> do
  let wss = sort $ map W.tag $ W.hidden ws ++ map  W.workspace (W.current ws : W.visible ws)
  gridselect conf (zip wss wss) >>= flip whenJust (windows . viewFn)

mykeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
       [ ((modm, xK_space), sendMessage NextLayout)
         -- navigation
       , ((modm, xK_l), windowGo R False)
       , ((modm, xK_h), windowGo L False)
       , ((modm, xK_j), windowGo D False)
       , ((modm, xK_k), windowGo U False)
       , ((modm .|. shiftMask, xK_l), windowSwap R False)
       , ((modm .|. shiftMask, xK_h), windowSwap L False)
       , ((modm .|. shiftMask, xK_j), windowSwap D False)
       , ((modm .|. shiftMask, xK_k), windowSwap U False)

         --stump-like stuff
       , ((modm, xK_t),submap . M.fromList $
         [ ((0, xK_c), spawn "urxvtc")
         , ((controlMask, xK_c), spawn "urxvtc")
         , ((0, xK_e), spawn "emacsclient -c")
         , ((0, xK_p), spawn "dmenu_run")
         , ((modm, xK_q), spawn $ "ghc -e ':m +XMonad Control.Monad System.Exit'"
                          ++ " -e 'flip unless exitFailure =<< recompile False'"
                          ++ " && xmonad --restart")
         , ((modm, xK_s), spawn "i3lock -e -t -i bsod.png")
         , ((0, xK_n), goToSelected gsConfigWin)
         , ((modm, xK_n), goToSelected gsConfigWin)
         , ((modm .|. shiftMask, xK_s), spawn $ "i3lock -e -t -i bsod.png"
                                        ++ " && sudo systemctl suspend")
           -- group map
         , ((modm, xK_g), myGridselectWorkspace gsConfigWS W.greedyView)
         , ((0, xK_g), submap . M.fromList $
           [ ((0, xK_d), removeWorkspace)
           , ((0, xK_c), addWorkspacePrompt defaultXPConfig)
           , ((0, xK_r), renameWorkspace defaultXPConfig)
           , ((0, xK_n), myGridselectWorkspace gsConfigWS W.greedyView)
           , ((shiftMask, xK_n), myGridselectWorkspace gsConfigWS
                                 (\ws -> W.greedyView ws . W.shift ws))
           , ((0, xK_s), gridselectWorkspace gsConfigWS W.greedyView)
           ])
           --layout map
         , ((modm, xK_n), gridselectLayout)
         , ((0, xK_l), submap . M.fromList $
           [ ((0, xK_n), gridselectLayout)
           , ((0, xK_comma), sendMessage (IncMasterN 1))
           , ((0, xK_period), sendMessage (IncMasterN (-1)))
           , ((0, xK_h), sendMessage Expand)
           , ((0, xK_l), sendMessage Shrink)
           ])
           --monitor map
         , ((0, xK_m), submap . M.fromList $
           [ ((0, xK_e), spawn "xrandr --output VGA1 --auto --left-of LVDS1")
           , ((0, xK_d), spawn "xrandr --output VGA1 --off")
           , ((0, xK_l), screenGo R False)
           , ((0, xK_h), screenGo L False)
           , ((0, xK_j), screenGo D False)
           , ((0, xK_k), screenGo U False)
           , ((shiftMask, xK_l), screenSwap R False)
           , ((shiftMask, xK_h), screenSwap L False)
           , ((shiftMask, xK_j), screenSwap D False)
           , ((shiftMask, xK_k), screenSwap U False)
           ])
         , ((0, xK_k), kill)
         ])
       ]

toggleBarKey XConfig {XMonad.modMask = modm} = (modm, xK_b)

batteryCmd = logCmd $ "/usr/bin/acpi | "
             ++ "sed -r"
             ++" 's/.*?: (.*)/\\1/;"
             ++" s/[dD]ischarging, [0-9]+%, ([0-9]+:[0-9]+:[0-9]+) .*/\\1-/;"
             ++" s/[cC]harging, [0-9]+%, ([0-9]+:[0-9]+:[0-9]+) .*/\\1+/;"
             ++" s/[cC]harged, /Charged/'"


xmobarConfig = xmobarPP {ppSep = " | "
                        , ppOrder = \(ws:lay:t:bat:_) -> [ws,lay,bat,t]
                        , ppExtras = [batteryCmd]
                        }

main :: IO()
main = (=<<) xmonad $ statusBar "xmobar" xmobarConfig toggleBarKey
       $ withNavigation2DConfig (defaultNavigation2DConfig { layoutNavigation = [("Full", centerNavigation)]
                                                           , unmappedWindowRect = [("Full", singleWindowRect)]
                                                           })
       $ ewmh
       $ defaultConfig { keys = mykeys
                       , terminal = "urxvtc"
                       , borderWidth = 1
                       , focusFollowsMouse = False
                       , layoutHook = myLayouts
                       , XMonad.workspaces = ["Default"]
                       , handleEventHook = handleEventHook defaultConfig
                                           <+> fullscreenEventHook
                       }
