import Common (evalPackets, parseFile, parsePackets)
import System.Environment (getArgs)

main =
  do
    args <- getArgs
    intList <- parseFile $ head args

    let packets = parsePackets intList []
    let results = maybe 0 evalPackets packets
    print results
