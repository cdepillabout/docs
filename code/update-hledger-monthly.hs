#!/usr/bin/env stack
-- stack --verbose --resolver lts-7.14 --install-ghc --no-system-ghc runghc --package classy-prelude --package hledger-lib

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude

main :: IO ()
main = do
    undefined
    -- marketPricesDir <- replaceTildeWithHomeDirString <$> getMarketPricesDir
    -- marketPriceFiles <- getMarketPriceFiles marketPricesDir
    -- yahooMarketPriceFiles <- filterOutNonYahooFiles marketPriceFiles
    -- updateMarketPricesForYahooFiles yahooMarketPriceFiles
