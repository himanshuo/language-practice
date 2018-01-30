module SecretHandshake (handshake) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)


handshake :: Int -> [String]
handshake n = binaryStringToHandshake $ intToBinaryString n


intToBinaryString :: Int -> String
intToBinaryString int = showIntAtBase 2 intToDigit int ""


binaryStringToHandshake :: String -> [String]
binaryStringToHandshake binaryString =
  let start = (length binaryString) - 1
      startList = start - 1
      end = 0
  in
    foldl (\acc i -> updateCurrentHandshake binaryString i acc) [] [start, startList..end]


updateCurrentHandshake :: String -> Int -> [String] -> [String]
updateCurrentHandshake binaryString index currentHandshake =
  let value = binaryString !! index
      position = (length binaryString) - index
      eventType = position `rem` 5
  in
      if value == '0'
      then currentHandshake
      else handleEventType eventType currentHandshake

handleEventType :: Int -> [String] -> [String]
handleEventType 0 currentHandshake = reverse currentHandshake
handleEventType 1 currentHandshake = currentHandshake ++ ["wink"]
handleEventType 2 currentHandshake = currentHandshake ++ ["double blink"]
handleEventType 3 currentHandshake = currentHandshake ++ ["close your eyes"]
handleEventType 4 currentHandshake = currentHandshake ++ ["jump"]
handleEventType _ _ = error "unhandled event type"
