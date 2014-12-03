module Main where

import Control.Monad.Identity

import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Criterion.Main

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.IConv as I

loremIpsum, loremIpsumUnicode :: String
-- https://en.wikipedia.org/wiki/Lorem_ipsum
loremIpsum = concat . replicate 128 $ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
-- http://www.geertvanderploeg.com/unicode-gen/
loremIpsumUnicode = concat . replicate 128 $ "L߀ｒèϻ ïρѕüｍ ԁòｌòｒ ѕｉｔ äｍéｔ, ｃòлｓêｃｔêｔúｒ ａԁìｐïѕｃìԉｇ ëｌïｔ, ｓéｄ Ꮷò éïüｓϻ߀ｄ ｔｅϻρ߀ｒ ìԉｃìԁíᏧûлｔ ûｔ ｌáƅ߀ｒｅ éｔ ｄ߀ｌòｒê ϻåǥｎà áｌïɋùå. Uｔ ëｎïϻ ａԁ ϻｉｎìϻ ｖｅлìäｍ, ｑúïѕ ｎ߀ѕｔｒüԁ èХêｒｃｉｔɑｔíòｎ ùｌɭàϻｃò ɭåｂ߀ｒïѕ ԉïｓí ûｔ àɭìɋûíｐ ｅх êà ｃ߀ｍｍòᏧ߀ ｃ߀ｎѕèɋûáｔ. Düïѕ ãüｔè ｉｒüｒé ｄ߀ｌòｒ ïｎ ｒｅρｒèհêｎᏧéｒìｔ ｉԉ ѵｏｌüｐｔãｔｅ ѵëɭｉｔ éｓｓê ｃｉɭｌûｍ ｄ߀ɭòｒè ëû ｆｕｇìɑｔ ԉúｌɭã ρɑｒìɑｔüｒ. Eӽｃëρｔéｕｒ ｓíԉｔ ｏｃｃɑéｃäｔ ｃùρíԁáｔâｔ лｏл ｐｒｏｉԁêлｔ, ѕúԉｔ íｎ ｃûɭｐä ｑｕｉ ߀ｆｆïｃｉä ｄèｓëｒùԉｔ ｍ߀ɭｌïｔ âԉïϻ íԁ ｅѕｔ ｌäƅｏｒùϻ."

main :: IO ()
main = defaultMain benchmarks

data SplitMode = None
               | Half
               | Quarter
               | ThreeBytes
               | TwoBytes
               | OneByte

convert :: (String -> B.ByteString) -> String -> String -> SplitMode -> String -> [B.ByteString]
convert encode from to split input = runIdentity $ CL.sourceList preparedInput $$ I.convert from to =$ CL.consume
    where
        encodedInput = encode input
        preparedInput = case split of
                            None       -> [encodedInput]
                            Half       -> halfByteString encodedInput
                            Quarter    -> concatMap halfByteString (halfByteString encodedInput)
                            ThreeBytes -> chunksOf 3 encodedInput
                            TwoBytes   -> chunksOf 2 encodedInput
                            OneByte    -> chunksOf 1 encodedInput

        halfByteString b = let (s, s') = B.splitAt (B.length b `div` 2) b
                           in [s, s']

        chunksOf c b | B.null b  = []
                     | otherwise = let (s, s') = B.splitAt c b
                                   in s : chunksOf c s'

benchmarks :: [Benchmark]
benchmarks = [
               bgroup "ascii"   [
                                  bench "32LE-8-N" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" None) loremIpsum
                                , bench "32LE-8-H" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" Half) loremIpsum
                                , bench "32LE-8-Q" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" Quarter) loremIpsum
                                , bench "32LE-8-3" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" ThreeBytes) loremIpsum
                                , bench "32LE-8-2" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" TwoBytes) loremIpsum
                                , bench "32LE-8-1" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" OneByte) loremIpsum

                                , bench "8-32LE-N" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" None) loremIpsum
                                , bench "8-32LE-H" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" Half) loremIpsum
                                , bench "8-32LE-Q" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" Quarter) loremIpsum
                                , bench "8-32LE-3" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" ThreeBytes) loremIpsum
                                , bench "8-32LE-2" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" TwoBytes) loremIpsum
                                , bench "8-32LE-1" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" OneByte) loremIpsum
                                ]
             , bgroup "unicode" [
                                  bench "32LE-8-N" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" None) loremIpsumUnicode
                                , bench "32LE-8-H" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" Half) loremIpsumUnicode
                                , bench "32LE-8-Q" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" Quarter) loremIpsumUnicode
                                , bench "32LE-8-3" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" ThreeBytes) loremIpsumUnicode
                                , bench "32LE-8-2" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" TwoBytes) loremIpsumUnicode
                                , bench "32LE-8-1" $ whnf (convert (TE.encodeUtf32LE . T.pack) "UTF-32LE" "UTF-8" OneByte) loremIpsumUnicode

                                , bench "8-32LE-N" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" None) loremIpsumUnicode
                                , bench "8-32LE-H" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" Half) loremIpsumUnicode
                                , bench "8-32LE-Q" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" Quarter) loremIpsumUnicode
                                , bench "8-32LE-3" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" ThreeBytes) loremIpsumUnicode
                                , bench "8-32LE-2" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" TwoBytes) loremIpsumUnicode
                                , bench "8-32LE-1" $ whnf (convert (TE.encodeUtf8 . T.pack) "UTF-8" "UTF-32LE" OneByte) loremIpsumUnicode
                                ]
             ]
