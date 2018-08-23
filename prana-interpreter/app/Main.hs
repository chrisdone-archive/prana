{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

-- |

module Main where

import Options.Applicative.Simple
import Paths_prana

main :: IO ()
main = do
  flagfile <- getDataFileName "flag.yaml"
  (_opts, cmd) <-
    simpleOptions
      "0.0.0"
      "prana"
      "Prana through Haskell modules"
      (pure ())
      (do pure ())
  pure ()
