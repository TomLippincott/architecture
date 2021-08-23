{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PostfixOperators #-}

module Main where

import Diagrams.Prelude hiding (scale, light)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.POVRay
import Architecture.Examples (slab, pergola)
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import System.Process
import Text.Printf (printf)

skyr = 0.0 :: Double
skyg = 0.0 :: Double
skyb = 0.0 :: Double
--sky = printf "background { color rgb <%.4f, %.4f, %.4f> }" skyr skyg skyb
sky = ""
main :: IO ()
main = do
  writeFile "test.pov" $ (renderDia POVRay POVRayOptions $ mconcat [pergola]) ++ "\n" ++ sky ++ "\n"
  (_, Just hout, _, p) <- createProcess (proc "/usr/bin/povray" ["-Itest.pov", "-Otest.png", "-W1024", "-H768", "-D0"]){ std_out=CreatePipe }
  waitForProcess p
  putStrLn "done."

