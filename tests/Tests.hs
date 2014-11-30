module Main where

import Control.Monad.Identity

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.IConv as I

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Divide a bytestring into up to 5 chunks
chunkByteString :: Int -> Int -> Int -> Int -> Int -> B.ByteString -> [B.ByteString]
chunkByteString n a b c d s = case n `mod` 5 of
                                    0 -> [s]
                                    1 -> let (s', s'') = B.splitAt (a `safeMod` B.length s) s
                                         in [s', s'']
                                    2 -> let (s', s'') = B.splitAt (a `safeMod` B.length s) s
                                             (t'', t''') = B.splitAt (b `safeMod` B.length s'') s''
                                         in [s', t'', t''']
                                    3 -> let (s', s'') = B.splitAt (a `safeMod` B.length s) s
                                             (t'', t''') = B.splitAt (b `safeMod` B.length s'') s''
                                             (u''', u'''') = B.splitAt (c `safeMod` B.length t''') t'''
                                         in [s', t'', u''', u'''']
                                    _ -> let (s', s'') = B.splitAt (a `safeMod` B.length s) s
                                             (t'', t''') = B.splitAt (b `safeMod` B.length s'') s''
                                             (u''', u'''') = B.splitAt (c `safeMod` B.length t''') t'''
                                             (v'''', v''''') = B.splitAt (d `safeMod` B.length u'''') u''''
                                         in [s', t'', u''', v'''', v''''']
                              where
                                safeMod _ 0 = 0
                                safeMod m o = abs $ m `mod` abs o

prop_identityASCIIToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityASCIIToUTF8 inString n a b c d = input == output
    where input = B.map (\w -> if w >= 128 then w - 128 else w) . BC.pack $ inString
          cInput = chunkByteString n a b c d input
          converted = runIdentity $ CL.sourceList cInput $$ I.convert "ASCII" "UTF-8" =$ CL.consume
          output = B.concat converted

prop_identityLatin1ToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityLatin1ToUTF8 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . BC.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "Latin1" "UTF-8" =$ CL.consume
          output = T.unpack . TE.decodeUtf8 . B.concat $ converted

prop_identityUTF8ToUTF16 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF8ToUTF16 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . TE.encodeUtf8 . T.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "UTF-8" "UTF-16LE" =$ CL.consume
          output = T.unpack . TE.decodeUtf16LE . B.concat $ converted

prop_identityUTF16ToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF16ToUTF8 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . TE.encodeUtf16LE . T.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "UTF-16LE" "UTF-8" =$ CL.consume
          output = T.unpack . TE.decodeUtf8 . B.concat $ converted

prop_identityUTF8ToUTF32 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF8ToUTF32 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . TE.encodeUtf8 . T.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "UTF-8" "UTF-32LE" =$ CL.consume
          output = T.unpack . TE.decodeUtf32LE . B.concat $ converted

prop_identityUTF16ToUTF32 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF16ToUTF32 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . TE.encodeUtf16LE . T.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "UTF-16LE" "UTF-32LE" =$ CL.consume
          output = T.unpack . TE.decodeUtf32LE . B.concat $ converted

prop_identityUTF32ToUTF16 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF32ToUTF16 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . TE.encodeUtf32LE . T.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "UTF-32LE" "UTF-16LE" =$ CL.consume
          output = T.unpack . TE.decodeUtf16LE . B.concat $ converted

prop_identityUTF32ToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF32ToUTF8 inString n a b c d = inString == output
    where input = chunkByteString n a b c d . TE.encodeUtf32LE . T.pack $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert "UTF-32LE" "UTF-8" =$ CL.consume
          output = T.unpack . TE.decodeUtf8 . B.concat $ converted

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
            testGroup "QuickCheck Data.Conduit.IConv" [
                  testProperty "identityASCIIToUTF8" prop_identityASCIIToUTF8
                , testProperty "identityLatin1ToUTF8" prop_identityLatin1ToUTF8
                , testProperty "identityUTF8ToUTF16" prop_identityUTF8ToUTF16
                , testProperty "identityUTF16ToUTF8" prop_identityUTF16ToUTF8
                , testProperty "identityUTF8ToUTF32" prop_identityUTF8ToUTF32
                , testProperty "identityUTF16ToUTF32" prop_identityUTF16ToUTF32
                , testProperty "identityUTF32ToUTF16" prop_identityUTF32ToUTF16
                , testProperty "identityUTF32ToUTF8" prop_identityUTF32ToUTF8
            ]
        ]
