{-# LANGUAGE ForeignFunctionInterface #-}

-- | A "Data.Conduit#t:Conduit" based on the iconv library for
-- converting a "Data.ByteString" from one character set encoding to another
module Data.Conduit.IConv
    (
      convert
    ) where

import Data.List (isPrefixOf, isInfixOf)
import Data.Char (toLower)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Control.Applicative ((<$>))

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

-- | Convert text from one named character encoding to another.
--
-- Encoding names can be e.g. @\"UTF-8\"@ or @\"ISO-8559-1\"@. Appending
-- @\"\/\/IGNORE\"@ to the output encoding will cause encoding errors to be
-- ignored and the characters to be dropped, @\"\/\/IGNORE,TRANSLIT\"@ will
-- cause them to be replaced by a replacement character.
--
-- Without this encoding errors will cause an exception.
--
-- On errors this will call "Control.Monad#v:fail"
convert :: Monad m => String -- ^ Name of input character encoding
                   -> String -- ^ Name of output character encoding
                   -> Conduit B.ByteString m B.ByteString
convert inputEncoding outputEncoding =
    let initialConvert = iconvConvert inputEncoding outputEncoding
    in run initialConvert

  where
    run f = do
        maybeInput <- await
        case maybeInput of
            Nothing    -> return ()
            Just input -> do
                            let res = f input
                            case res of
                                ConvertSuccess c f'                -> do
                                                                          CL.sourceList c
                                                                          run f'

                                ConvertUnsupportedConversionError  -> fail "Unsupported conversion"
                                ConvertUnexpectedOpenError s       -> fail ("Unexpected open error: " ++ s)
                                ConvertInvalidInputError           -> fail "Invalid input"
                                ConvertUnexpectedConversionError s -> fail ("Unexpected conversion error: " ++ s)

-- Stream based API around iconv()
data ConvertResult =
    ConvertSuccess [B.ByteString] (B.ByteString -> ConvertResult)
  | ConvertUnsupportedConversionError
  | ConvertUnexpectedOpenError String
  | ConvertInvalidInputError
  | ConvertUnexpectedConversionError String

iconvConvert :: String -> String -> B.ByteString -> ConvertResult
iconvConvert inputEncoding outputEncoding input =
    let eCtx = iconvOpen inputEncoding outputEncoding
    in
        case eCtx of
            Left UnsupportedConversion               -> ConvertUnsupportedConversionError
            Left (UnexpectedOpenError (Errno errno)) -> ConvertUnexpectedOpenError (show errno)
            Right ctx                                -> convertInput ctx B.empty [] input

  where
    filterNonEmpty = filter (not . B.null)

    convertInput ctx remaining converted newInput
      | B.null newInput  =
            ConvertSuccess (filterNonEmpty converted) (convertInput ctx remaining [])

      | B.null remaining =
            let res = iconv ctx newInput
            in
                case res of
                    Converted                 c r -> ConvertSuccess (filterNonEmpty $ converted ++ [c]) (convertInput ctx r [])
                    MoreData                  c r -> convertInput ctx B.empty (converted ++ [c]) r
                    InvalidInput              c _ -> ConvertSuccess (filterNonEmpty $ converted ++ [c]) (const ConvertInvalidInputError)
                    UnexpectedError (Errno errno) -> ConvertUnexpectedConversionError (show errno)

      | otherwise =
            let tmpInput = remaining `B.append` B.take 32 newInput
                res      = iconv ctx tmpInput
            in
                case res of
                    Converted                 c r -> let processed = B.length tmpInput - B.length r
                                                         consumedInput = processed - B.length remaining
                                                     in
                                                         if processed < B.length remaining then
                                                             ConvertSuccess (filterNonEmpty converted) (convertInput ctx (remaining `B.append` newInput) [])
                                                         else
                                                             convertInput ctx B.empty (converted ++ [c]) (B.drop consumedInput newInput)
                    MoreData                  c r -> let processed = B.length tmpInput - B.length r
                                                         consumedInput = processed - B.length remaining
                                                     in
                                                         if processed < B.length remaining then
                                                             ConvertSuccess (filterNonEmpty converted) (convertInput ctx (remaining `B.append` newInput) [])
                                                         else
                                                             convertInput ctx B.empty (converted ++ [c]) (B.drop consumedInput newInput)
                    InvalidInput              c _ -> ConvertSuccess (filterNonEmpty $ converted ++ [c]) (const ConvertInvalidInputError)
                    UnexpectedError (Errno errno) -> ConvertUnexpectedConversionError (show errno)


-- Thin wrapper around iconv_open()
data IConvT = IConvT (ForeignPtr IConvT) Double
data IConvOpenError =
    UnsupportedConversion
  | UnexpectedOpenError Errno

-- Some rather arbitrary numbers for the number of bytes
-- per character in different character encodings. This
-- is just to optimize allocations a bit but more accurate
-- numbers would be good to have.
bytesPerChar :: String -> Double
bytesPerChar encoding =
    case () of
        _ | "utf8"     `isInfixOf`  normalizedEncoding -> 1.3
        _ | "utf7"     `isInfixOf`  normalizedEncoding -> 1.5
          | "utf16"    `isInfixOf`  normalizedEncoding -> 2.0
          | "ucs2"     `isInfixOf`  normalizedEncoding -> 2.0
          | "utf32"    `isInfixOf`  normalizedEncoding -> 4.0
          | "ucs4"     `isInfixOf`  normalizedEncoding -> 4.0
          | "gb18030"  `isPrefixOf` normalizedEncoding -> 4.0
          | otherwise                                  -> 1.0
  where
    normalizedEncoding = filter (`notElem` "_-/") . map toLower $ encoding

iconvOpen :: String -> String -> Either IConvOpenError IConvT
iconvOpen inputEncoding outputEncoding = unsafePerformIO $ do
    ptr <- withCString inputEncoding  $ \inputEncodingPtr  ->
           withCString outputEncoding $ \outputEncodingPtr ->
                c_iconv_open outputEncodingPtr inputEncodingPtr
    case ptrToIntPtr ptr of
        (-1) -> do
                    errno <- getErrno
                    return $ Left $
                        if errno == eINVAL then
                            UnsupportedConversion
                        else
                            UnexpectedOpenError errno
        _    -> do
                    fPtr <- newForeignPtr c_iconv_close ptr
                    return $ Right (IConvT fPtr (bytesPerChar outputEncoding / bytesPerChar inputEncoding))

-- Thin wrapper around iconv()
data IConvResult =
    Converted B.ByteString B.ByteString
  | MoreData B.ByteString B.ByteString
  | InvalidInput B.ByteString B.ByteString
  | UnexpectedError Errno

iconv :: IConvT -> B.ByteString -> IConvResult
iconv (IConvT fPtr outRatio) input = unsafePerformIO $
    withForeignPtr fPtr                          $ \ptr             ->
    BU.unsafeUseAsCStringLen input               $ \(inPtr, inLeft) ->
    with inPtr                                   $ \inPtrPtr        ->
    with (fromIntegral inLeft)                   $ \inLeftPtr       ->
    let outLeft = max (ceiling (fromIntegral inLeft * outRatio) + 16) 4096 in
    mallocBytes outLeft >>=                        \outPtr          ->
    BU.unsafePackCStringLen (outPtr, outLeft)  >>= \output          ->   -- created here already to be garbage collected in case of async exceptions
    with outPtr                                  $ \outPtrPtr       ->
    with (fromIntegral outLeft)                  $ \outLeftPtr      -> do

        res <- c_iconv ptr inPtrPtr inLeftPtr outPtrPtr outLeftPtr

        inLeft'  <- fromIntegral <$> peek inLeftPtr
        outLeft' <- fromIntegral <$> peek outLeftPtr
        let output'   = B.take (outLeft - outLeft') output
            remaining = B.drop (inLeft - inLeft') input

        if res /= (-1) then
           return $ Converted output' remaining
        else do
           errno <- getErrno
           case () of
             _ | errno == e2BIG  -> return $
                                        if outLeft == outLeft' then          -- we processed nothing and it's still too big?!
                                            UnexpectedError errno
                                        else
                                            MoreData output' remaining
               | errno == eINVAL -> return $ Converted output' remaining   -- nothing converted here is no error as with future data we might be able to convert
               | errno == eILSEQ -> return $
                                        if outLeft == outLeft' then          -- we processed nothing
                                            UnexpectedError errno
                                        else
                                            InvalidInput output' remaining
               | otherwise       -> return $ UnexpectedError errno


-- Taken from Codec.Text.IConv
foreign import ccall unsafe "hsiconv.h hs_wrap_iconv_open"
  c_iconv_open :: CString  -- to code
               -> CString  -- from code
               -> IO (Ptr IConvT)

foreign import ccall unsafe "hsiconv.h hs_wrap_iconv"
  c_iconv :: Ptr IConvT
          -> Ptr (Ptr CChar)  -- in buf
          -> Ptr CSize        -- in buf bytes left
          -> Ptr (Ptr CChar)  -- out buf
          -> Ptr CSize        -- out buf bytes left
          -> IO CSize

foreign import ccall unsafe "hsiconv.h &hs_wrap_iconv_close"
  c_iconv_close :: FinalizerPtr IConvT

