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

prop_identityLatin1ToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityLatin1ToUTF8 = prop_identity BC.pack "Latin1" (T.unpack . TE.decodeUtf8) "UTF-8"

prop_identityUTF8ToUTF16 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF8ToUTF16 = prop_identity (TE.encodeUtf8 . T.pack) "UTF-8" (T.unpack . TE.decodeUtf16LE) "UTF-16LE"

prop_identityUTF16ToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF16ToUTF8 = prop_identity (TE.encodeUtf16LE . T.pack) "UTF-16LE" (T.unpack . TE.decodeUtf8) "UTF-8"

prop_identityUTF8ToUTF32 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF8ToUTF32 = prop_identity (TE.encodeUtf8 . T.pack) "UTF-8" (T.unpack . TE.decodeUtf32LE) "UTF-32LE"

prop_identityUTF16ToUTF32 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF16ToUTF32 = prop_identity (TE.encodeUtf16LE . T.pack) "UTF-16LE" (T.unpack . TE.decodeUtf32LE) "UTF-32LE"

prop_identityUTF32ToUTF16 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF32ToUTF16 = prop_identity (TE.encodeUtf32LE . T.pack) "UTF-32LE" (T.unpack . TE.decodeUtf16LE) "UTF-16LE"

prop_identityUTF32ToUTF8 :: String -> Int -> Int -> Int -> Int -> Int -> Bool
prop_identityUTF32ToUTF8 = prop_identity (TE.encodeUtf32LE . T.pack) "UTF-32LE" (T.unpack . TE.decodeUtf8) "UTF-8"

prop_identity ::    (String -> BC.ByteString)
                 -> I.CharacterEncoding
                 -> (BC.ByteString -> String)
                 -> I.CharacterEncoding
                 -> String
                 -> Int
                 -> Int
                 -> Int
                 -> Int
                 -> Int
                 -> Bool
prop_identity encode encodeTo decode decodeTo inString n a b c d = inString == output
    where input = chunkByteString n a b c d . encode $ inString
          converted = runIdentity $ CL.sourceList input $$ I.convert encodeTo decodeTo =$ CL.consume
          output = decode . B.concat $ converted

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
            testGroup "QuickCheck Data.Conduit.IConv" [
                  testProperty "identityLatin1ToUTF8" prop_identityLatin1ToUTF8
                , testProperty "identityUTF8ToUTF16"  prop_identityUTF8ToUTF16
                , testProperty "identityUTF16ToUTF8"  prop_identityUTF16ToUTF8
                , testProperty "identityUTF8ToUTF32"  prop_identityUTF8ToUTF32
                , testProperty "identityUTF16ToUTF32" prop_identityUTF16ToUTF32
                , testProperty "identityUTF32ToUTF16" prop_identityUTF32ToUTF16
                , testProperty "identityUTF32ToUTF8"  prop_identityUTF32ToUTF8
            ]
        ]
