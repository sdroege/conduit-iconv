-- | A "Data.Conduit#t:Conduit" based on "Codec.Text.IConv" for
-- converting a "Data.ByteString" from one character set encoding to another
module Codec.Text.IConv.Conduit
    (
      convert
    ) where

import Data.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Text.IConv as IConv
import Control.Exception (throw)
import Control.Monad (unless)

-- | Convert text from one named character encoding to another.
--
-- Encoding names can be e.g. @\"UTF-8\"@ or @\"ISO-8559-1\"@. Appending
-- @\"\/\/IGNORE\"@ to the output encoding will cause encoding errors to be
-- ignored and the characters to be dropped, @\"\/\/IGNORE,TRANSLIT\"@ will
-- cause them to be replaced by a replacement character.
--
-- Without this encoding errors will cause an exception.
convert :: Monad m => IConv.EncodingName -- ^ Name of input character encoding
                   -> IConv.EncodingName -- ^ Name of output character encoding
                   -> Conduit B.ByteString m B.ByteString
convert inputEncoding outputEncoding = run BL.empty
    where
        run prev = do
            input <- await
            case input of
                Nothing    -> unless (BL.null prev) $
                                    throw $ IConv.reportConversionError (IConv.IncompleteChar 0)
                Just bytes -> let chunks = prev `BL.append` BL.fromStrict bytes
                                  output = IConv.convertLazily inputEncoding outputEncoding chunks
                              in iterateOutput chunks output

        iterateOutput chunks output = case output of
            (o:os)  -> case o of
                        IConv.Span bytes                                    -> yield bytes >> iterateOutput chunks os
                        IConv.ConversionError e@(IConv.IncompleteChar offs) -> if null os then
                                                                                   run $ BL.drop (fromIntegral offs) chunks
                                                                               else
                                                                                   throw $ IConv.reportConversionError e
                        IConv.ConversionError e                             -> throw $ IConv.reportConversionError e
            _       -> run BL.empty

