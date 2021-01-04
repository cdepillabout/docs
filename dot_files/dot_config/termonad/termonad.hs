{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), set)
import Data.Colour.SRGB (sRGB24)
import Termonad.App (defaultMain)
import Termonad.Config
import Termonad.Config.Colour

main :: IO ()
main = do
  print "running termonad from default file!!!"
  let colConf =
        defaultColourConfig
          { cursorBgColour = Set $ createColour 204 0 0
          , palette =
              let myStandardColors =
                    setAtList8 4 (createColour 120 120 250) $
                    defaultStandardColours
                  myLightCols =
                    setAtList8 4 (createColour 150 150 250) $
                    defaultLightColours
              in ExtendedPalette myStandardColors myLightCols
          -- , foregroundColour = Set (createColour 220 50 50)
          -- , backgroundColour = Set (createColour 50 50 50)
          }
  colExt <- createColourExtension colConf
  let tmConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig =
                    FontConfig
                      { fontFamily = "DejaVu Sans Mono"
                      , fontSize = FontSizePoints 13
                      }
                , showScrollbar = ShowScrollbarAlways
                , showTabBar = ShowTabBarAlways
                , scrollbackLen = 20000
                , boldIsBright = False
                -- , confirmExit = False
                }
          }
        `addColourExtension` colExt
  defaultMain tmConf
