module Main(
  main
, ersas
) where

import Control.Monad((>>=))
import Data.Aviation.Ersa.Concat(ErsaDocument(ErsaDocument), ErsaCurrentPending(ErsaCurrent, ErsaPending), defaultersaconcat, traverseExitCodes)
import System.Environment(getArgs)
import System.Exit(ExitCode(ExitFailure), exitWith)
import System.IO(IO, hPutStrLn, stderr)

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        p:d:v:z ->
          defaultersaconcat (ErsaDocument p d v ( case z of
                                                    [] ->
                                                      ErsaCurrent
                                                    _ ->
                                                      ErsaPending)) >>= exitWith
        [] ->
          traverseExitCodes defaultersaconcat ersas >>= exitWith
        _ ->
          do  hPutStrLn stderr "Enter zero arguments for default ERSA list or three arguments for single ERSA <page> <date (dd-Mmm-yyyy where Mmm is the first three letters of the Julian month name)> <version> [a fourth argument implies the ERSA is pending]"
              exitWith (ExitFailure 65535)

ersas ::
  [ErsaDocument]
ersas =
  [
    ErsaDocument "40" "18-Aug-2016" "1" ErsaCurrent
  , ErsaDocument "40" "10-Nov-2016" "0" ErsaPending
  , ErsaDocument "40" "10-Nov-2016" "2" ErsaPending
  ]
