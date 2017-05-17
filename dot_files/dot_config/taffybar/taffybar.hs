{-# OPTIONS_GHC -Wall #-}

import System.Taffybar
       (defaultTaffybar, defaultTaffybarConfig, endWidgets, startWidgets)
import System.Taffybar.Systray (systrayNew)
import System.Taffybar.SimpleClock (textClockNew)
import System.Taffybar.TaffyPager
       (defaultPagerConfig, taffyPagerNew)
import System.Taffybar.Widgets.PollingGraph
       (defaultGraphConfig, graphDataColors, graphLabel, pollingGraphNew)
import System.Information.CPU (cpuLoad)

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  pure [totalLoad, systemLoad]

main :: IO ()
main = do
  let cpuCfg =
        defaultGraphConfig
          { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5) ]
          , graphLabel = Just "cpu"
          }
      clock =
        textClockNew
          Nothing
          "<span fgcolor='orange'>%a %b %_d %H:%M</span>"
          1
      pager = taffyPagerNew defaultPagerConfig
      tray = systrayNew
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  defaultTaffybar
    defaultTaffybarConfig
      { startWidgets = [pager]
      , endWidgets = [tray, clock, cpu]
      }
