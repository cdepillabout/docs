{-# OPTIONS_GHC -Wall #-}

import System.Information.CPU (cpuLoad)
import System.Information.Memory (parseMeminfo, memoryUsedRatio)
import System.Taffybar
       (barHeight, defaultTaffybar, defaultTaffybarConfig, endWidgets,
        startWidgets)
import System.Taffybar.Battery (batteryBarNew, defaultBatteryConfig)
-- import System.Taffybar.DiskIOMonitor (dioMonitorNew)
import System.Taffybar.FreedesktopNotifications
       (defaultNotificationConfig, notifyAreaNew)
import System.Taffybar.Systray (systrayNew)
import System.Taffybar.SimpleClock (textClockNew)
import System.Taffybar.TaffyPager
       (defaultPagerConfig, taffyPagerNew)
import System.Taffybar.Widgets.PollingGraph
       (defaultGraphConfig, graphDataColors, graphLabel, pollingGraphNew)


memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  pure [memoryUsedRatio mi]

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
      memCfg =
        defaultGraphConfig
          { graphDataColors = [(1, 0, 0, 1)]
          , graphLabel = Just "mem"
          }
      -- diskCfg =
      --   defaultGraphConfig
      --     { {- graphDataColors = [(1, 0, 0, 1)]
      --     , -} graphLabel = Just "disk"
      --     }
      clock =
        textClockNew
          Nothing
          "<span fgcolor='orange'>%a %b %_d %H:%M</span>"
          1
      pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      tray = systrayNew
      cpu = pollingGraphNew cpuCfg 2 cpuCallback
      mem = pollingGraphNew memCfg 5 memCallback
      -- disk = dioMonitorNew diskCfg 2 "sda"
      battery = batteryBarNew defaultBatteryConfig 2
  defaultTaffybar
    defaultTaffybarConfig
      { barHeight = 20
      , startWidgets = [pager, note]
      , endWidgets = [tray, clock, battery, mem, cpu]
      }
