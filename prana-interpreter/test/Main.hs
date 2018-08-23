{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Exception
import           Data.OrdGraph
import qualified Data.Set as Set
import qualified GHCHarness as GHC
import           Prana.Interpreter
import qualified Prana.FrontendPlugin as Prana
import           Prana.Types
import           System.Directory
import           System.IO.Temp
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  pure ()
