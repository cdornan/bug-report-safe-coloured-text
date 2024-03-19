{-# LANGUAGE Haskell2010                #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.ByteString                as B
import           Data.Word
import           Fmt
import           Text.Colour


main :: IO ()
main = do
  putStr "this 'hello' has a magenta background: "
  test 0xff Nothing
  putStr "\n"
  putStr "this 'hello' should have a red background: "
  test 0x00 Nothing
  putStr "\n"
  putStr "with the final CSI zero paremeter explicitly encoded this 'hello' has a red background: "
  test 0x00 $ Just "\ESC[48;2;255;;0mhello\ESC[m"
  putStr "\n"

test :: Word8 -> Maybe B.ByteString -> IO ()
test b mb = do
    B.putStr out
    putStr "\n"
    mapM_ dump $ B.unpack out
  where
    out, out_ :: B.ByteString
    out  = maybe out_ id mb
    out_ = renderChunksUtf8BS With24BitColours $ (:[])
      Chunk 
            { chunkText             = "hello"
            , chunkItalic           = Nothing
            , chunkConsoleIntensity = Nothing
            , chunkUnderlining      = Nothing
            , chunkBlinking         = Nothing
            , chunkForeground       = Nothing
            , chunkBackground       = Just (Colour24Bit 255 0 b)
            }

    dump :: Word8 -> IO ()
    dump w = fmt $ "  "+|hex w|+" - "+||chr w||+"\n"

hex :: Word8 -> Builder
hex = padLeftF 2 '0' . hexF

chr :: Word8 -> Char
chr = toEnum . fromEnum
