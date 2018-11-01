import Data.List
import System.Environment
import System.Exit
import System.IO
import System.Process

main = do
  args <- getArgs
  if any (== "--frontend") args
    then do
      (result, out, err) <-
        readProcessWithExitCode
          "/root/ghc_build/ghc-8.0/inplace/bin/ghc-stage2"
          (filter (\c -> notElem c ["-c", "-M"]) args)
          ""
      case result of
        ExitSuccess {} ->
          rawSystem
            "/root/ghc_build/ghc-8.0/inplace/bin/ghc-stage1"
            (removeKey "--frontend" args) >>=
          exitWith
        _ -> do
          hPutStrLn stderr err
          hPutStrLn stdout out
          exitWith result
    else rawSystem "/root/ghc_build/ghc-8.0/inplace/bin/ghc-stage1" args

removeKey k (x:y:xs) | x == k = xs
removeKey k (x:xs) = x : removeKey k xs
removeKey _ [] = []
