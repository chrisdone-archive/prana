{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Distribution.Simple

main = defaultMainWithHooks autoconfUserHooks
