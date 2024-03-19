{-# LANGUAGE Haskell2010                #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.ByteString                as B
import           Data.Word
import           Fmt
import           Text.Colour
import           Text.Colour.Code


main :: IO ()
main = do
  putStr "this 'hello' has a magenta background: "
  test $ Left $ Colour24Bit 0xff 0x00 0xff
  putStr "\n"
  putStr "this 'hello' should have a red background: "
  test $ Left $ Colour24Bit 0xff 0x00 0x00
  putStr "\n"
  putStr "with the final CSI zero paremeter explicitly encoded this 'hello' has a red background: "
  test $ Right $ "\ESC[48;2;255;;0mhello\ESC[m"
  putStr "\n"
  putStr "with ye olde 4-bit colours this 'hello' has a (bright) red background: "
  test $ Left $ Colour8 Bright Red
  putStr "\n"

test :: Either Colour B.ByteString -> IO ()
test ei = do
    B.putStr out
    putStr "\n"
    mapM_ dump $ B.unpack out
  where
    out:: B.ByteString
    out = either rdr id ei

    rdr :: Colour -> B.ByteString
    rdr c = renderChunksUtf8BS With24BitColours $ (:[])
      Chunk 
            { chunkText             = "hello"
            , chunkItalic           = Nothing
            , chunkConsoleIntensity = Nothing
            , chunkUnderlining      = Nothing
            , chunkBlinking         = Nothing
            , chunkForeground       = Nothing
            , chunkBackground       = Just c
            }

    dump :: Word8 -> IO ()
    dump w = fmt $ "  "+|hex w|+" - "+||chr w||+"\n"

hex :: Word8 -> Builder
hex = padLeftF 2 '0' . hexF

chr :: Word8 -> Char
chr = toEnum . fromEnum
