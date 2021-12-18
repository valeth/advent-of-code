{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Common
  ( parseFile,
    parseRootPacket,
    sumPacketVersions,
    evalPacket,
  )
where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Numeric (readHex)
import System.IO (readFile)
import Text.Printf (printf)

type Bit = Int

data Op = Sum | Product | Min | Max | Gt | Lt | Eq
  deriving (Show)

data PacketData = Literal Int | Operator Op [Packet]
  deriving (Show)

data Packet = Packet
  { version :: Int,
    content :: PacketData
  }
  deriving (Show)

-- Parsing

parseFile :: String -> IO [Bit]
parseFile path = do
  content <- readFile path
  let intList = concat (mapMaybe hexCharToBits content)
  return intList

hexCharToBits :: Char -> Maybe [Bit]
hexCharToBits char =
  case readHex [char] of
    [(val, _)] -> Just $ intToBits (val :: Int)
    _ -> Nothing
  where
    intToBits i = map digitToInt $ printf "%04b" i

bitsToInt :: [Bit] -> Int
bitsToInt [] = 0
bitsToInt bs =
  bitsToInt' bs (length bs - 1)
  where
    bitsToInt' [] _ = 0
    bitsToInt' (0 : bs) p = bitsToInt' bs (p - 1)
    bitsToInt' (1 : bs) p = (2 ^ p) + bitsToInt' bs (p - 1)

parseRootPacket :: [Bit] -> Maybe Packet
parseRootPacket bits = do
  (_, packet) <- parsePacket bits
  Just packet

parsePacket :: [Bit] -> Maybe (Int, Packet)
parsePacket [] = Nothing
parsePacket bits = do
  (consumed, content) <- case kind of
    0 -> parseOperator (drop 6 bits) Sum (6, [])
    1 -> parseOperator (drop 6 bits) Product (6, [])
    2 -> parseOperator (drop 6 bits) Min (6, [])
    3 -> parseOperator (drop 6 bits) Max (6, [])
    4 -> parseLiteral (drop 6 bits) (6, [])
    5 -> parseOperator (drop 6 bits) Gt (6, [])
    6 -> parseOperator (drop 6 bits) Lt (6, [])
    7 -> parseOperator (drop 6 bits) Eq (6, [])
  let packet = Packet {version, content}
  Just (consumed, packet)
  where
    version = bitsToInt $ take 3 bits
    kind = bitsToInt $ take 3 $ drop 3 bits

parseOperator :: [Bit] -> Op -> (Int, [Packet]) -> Maybe (Int, PacketData)
parseOperator bits op (consumed, memo) =
  case bits of
    (0 : bs) -> do
      let totalBitLength = bitsToInt (take 15 bs)
      let opConsumed = consumed + 16
      let nextBits = take totalBitLength (drop 15 bs)
      packets <- parsePacketsDense nextBits totalBitLength []
      let newConsumed = opConsumed + totalBitLength
      Just (newConsumed, Operator op packets)
    (1 : bs) -> do
      let subPacketCount = bitsToInt (take 11 bs)
      let opConsumed = consumed + 12
      let nextBits = drop 11 bs
      (pkgConsumed, packets) <- subPackets nextBits subPacketCount (0, [])
      let newConsumed = opConsumed + pkgConsumed
      Just (newConsumed, Operator op packets)
    _ ->
      Nothing
  where
    parsePacketsDense bs 0 packets = Just packets
    parsePacketsDense bs rem packets = do
      (pkgConsumed, packet) <- parsePacket bs
      parsePacketsDense (drop pkgConsumed bs) (rem - pkgConsumed) (packets ++ [packet])
    subPackets bs 0 (consumed, packets) = Just (consumed, packets)
    subPackets bs n (consumed, packets) = do
      (pkgConsumed, packet) <- parsePacket bs
      let nextBits = drop pkgConsumed bs
      subPackets nextBits (n - 1) (consumed + pkgConsumed, packets ++ [packet])

parseLiteral :: [Bit] -> (Int, [Bit]) -> Maybe (Int, PacketData)
parseLiteral bits (consumed, memo) =
  case bits of
    (1 : bs) -> parseLiteral (drop 4 bs) (consumed + 5, memo ++ take 4 bs)
    (0 : bs) -> Just (consumed + 5, Literal (bitsToInt (memo ++ take 4 bs)))
    _ -> Nothing

-- Results

sumPacketVersions :: Packet -> Int -> Int
sumPacketVersions Packet {version, content = Literal _} memo = memo + version
sumPacketVersions Packet {version, content = Operator _ packets} memo =
  version + sum (map (`sumPacketVersions` memo) packets)

evalPacket :: Packet -> Int
evalPacket Packet {content} =
  case content of
    Literal val -> val
    Operator Sum packets -> sum (map evalPacket packets)
    Operator Product packets -> product (map evalPacket packets)
    Operator Min packets -> minimum (map evalPacket packets)
    Operator Max packets -> maximum (map evalPacket packets)
    Operator Gt [x, y] -> evalBinOp (>) x y
    Operator Lt [x, y] -> evalBinOp (<) x y
    Operator Eq [x, y] -> evalBinOp (==) x y
  where
    evalBinOp f x y =
      let px = evalPacket x
          py = evalPacket y
       in if px `f` py then 1 else 0
