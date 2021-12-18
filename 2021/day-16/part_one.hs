import Common (parseFile, parsePackets, sumPacketVersions)
import System.Environment (getArgs)

main = do
  args <- getArgs
  intList <- parseFile $ head args

  let packets = parsePackets intList []
  let results = maybe 0 (`sumPacketVersions` 0) packets
  print results
