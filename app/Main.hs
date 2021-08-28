module Main where

import Prelude hiding (Unwrapped, Wrapped)
import Data.Maybe (fromMaybe)
import Diagrams.Prelude hiding (scale, light, Unwrapped, Wrapped)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.POVRay
import Architecture.Examples (slab, pergola)
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import System.Process
import Text.Printf (printf)
import Data.Text
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import System.IO (openTempFile, hPutStr, hClose, hFlush)
import System.Directory (getTemporaryDirectory, removeFile)
data Parameters w = Parameters { outputFile :: w ::: String <?> "Output file"
                               , w :: w ::: Maybe Int <?> "Image width"
                               , h :: w ::: Maybe Int <?> "Image height"
                               }
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

--main :: IO ()
--main = do
--  ps <- unwrapRecord "Do PPM-related stuff: all data files should be tab-separated lines of the form  ID<TAB>LABEL<TAB>TEXT  Specify a subcommand with '--help' to see its options."



main :: IO ()
--main = mainWith slab
main = do
  opts <- unwrapRecord "Render an architectural plan"
  let width = fromMaybe 1024 (w opts)
      height = fromMaybe 768 (w opts)
      output = outputFile opts
  tpath <- getTemporaryDirectory
  (path, handle) <- openTempFile tpath "architecture.pov"
  hPutStr handle $ (renderDia POVRay POVRayOptions $ mconcat [pergola]) ++ "\n"
  hClose handle
  (_, Just hout, _, p) <- createProcess (proc "/usr/bin/povray" [ printf "-I%s" path
                                                                , printf "-O%s" output
                                                                , printf "-W%d" width
                                                                , printf "-H%d" height
                                                                , "-D0"
                                                                ]){ std_out=CreatePipe }
  waitForProcess p
  removeFile path
  putStrLn "done."

