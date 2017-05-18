{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Monoid ((<>))
import Graphics.UI.Gtk (Widget, widgetShowAll)
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
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)


memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  pure [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  pure [totalLoad, systemLoad]

cpuTempNew :: Double -> IO Widget
cpuTempNew pollSeconds = do
  let temp = fmap tempLabel getCPUTemp
  label <- pollingLabelNew "" pollSeconds temp
  widgetShowAll label
  pure label
  where
    getCPUTemp :: IO Int
    getCPUTemp = do
      temp <-
        readFile "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon1/temp1_input"
      pure $ read temp `div` 1000

    tempLabel :: Int -> String
    tempLabel temp =
      let color =
            if | temp > 70 -> "red"
               | temp > 62 -> "orange"
               | temp > 54 -> "white"
               | temp > 46 -> "green"
               | otherwise -> "blue"
      in "temp <span fgcolor='" <> color <> "'>" <> show temp <> "</span>C"

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
      temp = cpuTempNew 5
  defaultTaffybar
    defaultTaffybarConfig
      { barHeight = 20
      , startWidgets = [pager, note]
      , endWidgets = [tray, clock, battery, temp, cpu, mem]
      }
