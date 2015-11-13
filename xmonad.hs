import qualified Data.Map as M
import Data.List (sort)
import XMonad hiding ((|||))
import XMonad.StackSet as W
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators ((|||),JumpToLayout(..))
import XMonad.Util.Loggers
import Text.Megaparsec hiding (hidden)
import Text.Megaparsec.String

data Remote = Remote String (Maybe String) deriving (Read, Show)
data DirSpec = DirSpec [Remote] String deriving (Read, Show)

parseDirSpec :: String -> Either ParseError DirSpec
parseDirSpec input = parse dirSpec "(XMonad input)" input

dirSpec :: Parser DirSpec
dirSpec = space >> many (noneOf " ") >> space >> DirSpec
          <$> many (try $ remote <* (char ':')) <*> many (noneOf " ")

remote :: Parser Remote
remote =  Remote <$> many (noneOf ":#")
          <*> choice [ char '#' >> Just <$> some numberChar, pure Nothing ]

-- needs to be in quotes, escaped
showSSHremotes :: [Remote] -> String
showSSHremotes [] = ""
showSSHremotes ((Remote host Nothing):rs) = "ssh " ++ host ++ " "
                                            ++ showSSHremotes rs
showSSHremotes ((Remote host (Just port)):rs) = "ssh " ++ host
                                                ++ " -p " ++ port ++ " "
                                                ++ showSSHremotes rs

-- needs to be in quotes, escaped
showtermSsh :: String -> DirSpec -> String
showtermSsh termCommand (DirSpec [] "") = termCommand
showtermSsh termCommand (DirSpec [] dir) =
  termCommand ++ " -e bash -c \'mkdir -p " ++ dir
  ++ "; cd " ++ dir
  ++ "; zsh --login \'"
showtermSsh termCommand (DirSpec remotes "") =
  termCommand ++" -e  bash -c \'" ++ showSSHremotes remotes ++ "\'"
showtermSsh termCommand (DirSpec remotes dir) =
  termCommand ++" -e bash -c \'" ++ showSSHremotes remotes
  ++ " -t mkdir -p " ++ dir
  ++ "; cd " ++ dir
  ++ "; bash --login \'"

showEmacsRemotes :: [Remote] -> String
showEmacsRemotes [] = ""
showEmacsRemotes ((Remote host Nothing):rs) =
  "ssh:" ++ host ++ ":" ++ showEmacsRemotes rs
showEmacsRemotes ((Remote host (Just port)):rs) =
  "ssh:" ++ host ++ "#" ++ port ++ ":" ++ showEmacsRemotes rs

showEmacsSsh :: String -> DirSpec -> String
showEmacsSsh emacsCmd (DirSpec [] "") = emacsCmd
showEmacsSsh emacsCmd (DirSpec [] dir) = emacsCmd ++ " " ++ dir
showEmacsSsh emacsCmd (DirSpec remotes dir) =
  emacsCmd ++ " /" ++ showEmacsRemotes remotes ++ dir

remThing ::  (String -> DirSpec -> String) -> String -> X ()
remThing showfn termCommand =
  gets windowset >>= term' . parseDirSpec . tag . workspace . current
  where term' (Right ds) = spawn $ showfn termCommand ds
        term' (Left  _ ) = spawn termCommand

remTerm :: String -> X ()
remTerm = remThing showtermSsh

remEmacs :: String -> X ()
remEmacs = remThing showEmacsSsh

gsConfigWS :: GSConfig WorkspaceId
gsConfigWS = def
gsConfigWin :: GSConfig Window
gsConfigWin = def
gsConfigLay :: GSConfig [Char]
gsConfigLay = def

layouts :: [String]
layouts = ["Tall", "Full", "Grid", "3 by 2", "Three Column"]

myLayouts = noBorders (Full) ||| tiled ||| Grid
            ||| renamed [Replace "3 by 2"] ( GridRatio (5/4))
            ||| renamed [Replace "Three Column"] (ThreeCol 1 (3/100) (1/3))
  where
    tiled = Tall 1 (3/100) (1/2)

gridselectLayout :: X ()
gridselectLayout = gridselect gsConfigLay (zip layouts layouts)
                   >>= flip whenJust (sendMessage . JumpToLayout)

myGridselectWorkspace :: GSConfig WorkspaceId
                      -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
myGridselectWorkspace conf viewFn = withWindowSet $ \ws -> do
  let wss = sort $ map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
  gridselect conf (zip wss wss) >>= flip whenJust (windows . viewFn)

mykeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
mykeys (XConfig {XMonad.modMask = modm}) = M.fromList
       [ ((modm, xK_l), windowGo R False)
       , ((modm, xK_h), windowGo L False)
       , ((modm, xK_j), windowGo D False)
       , ((modm, xK_k), windowGo U False)
       , ((modm .|. shiftMask, xK_l), windowSwap R False)
       , ((modm .|. shiftMask, xK_h), windowSwap L False)
       , ((modm .|. shiftMask, xK_j), windowSwap D False)
       , ((modm .|. shiftMask, xK_k), windowSwap U False)

       , ((modm, xK_t),submap . M.fromList $
         [ ((0, xK_c), remTerm "urxvtc")
         , ((modm, xK_c), spawn "urxvtc")
         , ((0, xK_e), remEmacs "emacsclient -c")
         , ((modm, xK_e), spawn "emacsclient -c")
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
         , ((modm .|. shiftMask, xK_g), myGridselectWorkspace gsConfigWS
                                        (\ws -> W.greedyView ws . W.shift ws))
         , ((0, xK_g), submap . M.fromList $
           [ ((0, xK_k), removeWorkspace)
           , ((0, xK_c), addWorkspacePrompt def)
           , ((0, xK_r), renameWorkspace def)
           , ((0, xK_n), myGridselectWorkspace gsConfigWS W.greedyView)
           , ((shiftMask, xK_n), myGridselectWorkspace gsConfigWS
                                 (\ws -> W.greedyView ws . W.shift ws))
           , ((0, xK_s), gridselectWorkspace gsConfigWS W.greedyView)
           ])
           --layout map
         , ((modm, xK_l), gridselectLayout)
         , ((0, xK_l), submap . M.fromList $
           [ ((0, xK_n), gridselectLayout)
           , ((0, xK_comma), sendMessage (IncMasterN 1))
           , ((0, xK_period), sendMessage (IncMasterN (-1)))
           , ((0, xK_h), sendMessage Expand)
           , ((0, xK_l), sendMessage Shrink)
           ])
           --monitor map
         , ((0, xK_m), submap . M.fromList $
           [ ((0, xK_e), spawn "xrandr --output VGA1 --auto --left-of LVDS1"
             >> rescreen)
           , ((0, xK_d), spawn "xrandr --output VGA1 --off"
             >> rescreen)
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

toggleBarKey :: XConfig t -> (KeyMask, KeySym)
toggleBarKey XConfig {XMonad.modMask = modm} = (modm, xK_b)

batteryCmd :: Logger
batteryCmd = logCmd $ "/usr/bin/acpi | "
             ++ "sed -r"
             ++" 's/.*?: (.*)/\\1/;"
             ++" s/[dD]ischarging, [0-9]+%, ([0-9]+:[0-9]+:[0-9]+) .*/\\1-/;"
             ++" s/[cC]harging, [0-9]+%, ([0-9]+:[0-9]+:[0-9]+) .*/\\1+/;"
             ++" s/[cC]harged, /Charged/'"

xmobarConfig :: PP
xmobarConfig = xmobarPP {ppSep = " | "
                        , ppOrder = \(ws:lay:t:bat:_) -> [ws,lay,bat,t]
                        , ppExtras = [batteryCmd]
                        }

main :: IO()
main = (=<<) xmonad $ statusBar "xmobar" xmobarConfig toggleBarKey
       $ withNavigation2DConfig ( def { layoutNavigation = [("Full", centerNavigation)]
                                      , unmappedWindowRect = [("Full", singleWindowRect)]
                                      })
       $ ewmh
       $ def { keys = mykeys
             , terminal = "urxvtc"
             , borderWidth = 1
             , focusFollowsMouse = False
             , layoutHook = myLayouts
             , XMonad.workspaces = ["Default"]
             , handleEventHook = handleEventHook def <+> fullscreenEventHook
             }
