module Main where

import Common (parseFile, parseRootPacket, sumPacketVersions)
import System.Environment (getArgs)

main = do
  args <- getArgs
  intList <- parseFile $ head args

  let rootPacket = parseRootPacket intList
  let results = maybe 0 (`sumPacketVersions` 0) rootPacket
  print results
