module Main where

import Common (evalPacket, parseFile, parseRootPacket)
import System.Environment (getArgs)

main = do
  args <- getArgs
  intList <- parseFile $ head args

  let rootPacket = parseRootPacket intList
  let results = maybe 0 evalPacket rootPacket
  print results
