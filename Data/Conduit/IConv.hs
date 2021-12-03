{-# LANGUAGE ForeignFunctionInterface #-}

-- | A `Conduit` based on the iconv library for
-- converting a `Data.ByteString` from one character set encoding to another
module Data.Conduit.IConv
    (
      CharacterEncoding
    , convert
    ) where

import Data.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Control.Applicative ((<$>))

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (mask_)

-- | Type synonym for character encoding names
--
-- See `convert` for details.
type CharacterEncoding = String

-- | Convert text from one named character encoding to another.
--
-- Encoding names can be e.g. @\"UTF-8\"@ or @\"ISO-8559-1\"@. Appending
-- @\"\/\/IGNORE\"@ to the output encoding will cause encoding errors to be
-- ignored and the characters to be dropped, @\"\/\/IGNORE,TRANSLIT\"@ will
-- cause them to be replaced by a replacement character.
--
-- Without this encoding errors will cause an exception.
--
-- On errors this will call `fail`
convert :: MonadFail m => CharacterEncoding -- ^ Name of input character encoding
                       -> CharacterEncoding -- ^ Name of output character encoding
                       -> Conduit B.ByteString m B.ByteString
convert inputEncoding outputEncoding = run initialConvert
  where
    initialConvert = iconvConvert inputEncoding outputEncoding

    run f = do
        maybeInput <- await
        case maybeInput of
            Nothing    -> return ()
            Just input -> do
                            let res = f input
                            case res of
                                ConvertSuccess c f'                -> yield c >> run f'
                                ConvertUnsupportedConversionError  -> fail "Unsupported conversion"
                                ConvertUnexpectedOpenError s       -> fail ("Unexpected open error: " ++ s)
                                ConvertInvalidInputError           -> fail "Invalid input"
                                ConvertUnexpectedConversionError s -> fail ("Unexpected conversion error: " ++ s)

-- Stream based API around iconv()
data ConvertResult =
    ConvertSuccess B.ByteString (B.ByteString -> ConvertResult)
  | ConvertUnsupportedConversionError
  | ConvertUnexpectedOpenError String
  | ConvertInvalidInputError
  | ConvertUnexpectedConversionError String

iconvConvert :: CharacterEncoding -> CharacterEncoding -> B.ByteString -> ConvertResult
iconvConvert inputEncoding outputEncoding input =
    let eCtx = iconvOpen inputEncoding outputEncoding
    in
        case eCtx of
            Left UnsupportedConversion               -> ConvertUnsupportedConversionError
            Left (UnexpectedOpenError (Errno errno)) -> ConvertUnexpectedOpenError (show errno)
            Right ctx                                -> convertInput ctx B.empty input

  where
    -- Converts newInput with the given context
    --
    -- If converted is not empty, this will be prepended to
    -- the converted output. This will happen if we're called
    -- after the remainder of a previous conversion was consumed.
    -- converted is the result of the remainder conversion.
    --
    -- If this conversion results in a remainder we return any
    -- results we got so far but will use convertInputWithRemaining
    -- with the remainder for the next call.
    convertInput ctx converted newInput
      | B.null newInput  =
            ConvertSuccess converted (convertInput ctx B.empty)

      | otherwise =
            let res = iconv ctx newInput converted
            in
                case res of
                    Converted                 c r -> ConvertSuccess c (convertInputWithRemaining ctx r)
                    InvalidInput              c _ -> if B.null c then
                                                         ConvertInvalidInputError
                                                     else
                                                         -- Converted a bit but detected invalid input afterwards.
                                                         -- Let's return what was converted so far and fail with
                                                         -- the next call
                                                         ConvertSuccess c (const ConvertInvalidInputError)
                    UnexpectedError (Errno errno) -> ConvertUnexpectedConversionError (show errno)

    -- Convert any remainder from a previous conversion. We do
    -- this by first appending the first 32 bytes of the newInput
    -- to the remainder. With 32 bytes additionally we should be
    -- able to convert the remainder from any possible charset
    -- encoding that has 32 *bytes* per character at maximum.
    -- 4 would've been enough too probably.
    --
    -- If the conversion was successful, we take the remaining
    -- newInput and the converted remainder, and hand it over
    -- to the previous conversion function. Which then will
    -- convert the remaining newInput and prepend the converted
    -- remainder to it.
    convertInputWithRemaining ctx remaining newInput
      | B.null remaining =
            convertInput ctx B.empty newInput

      | B.null newInput  =
            ConvertSuccess B.empty (convertInputWithRemaining ctx remaining)

      | otherwise =
            let tmpInput = remaining `B.append` B.take 32 newInput
                res      = iconv ctx tmpInput B.empty
            in
                case res of
                    Converted                 c r -> if processed < B.length remaining then
                                                         -- Didn't convert the complete remainder. Let's try again next
                                                         -- time with the additional newInput
                                                         ConvertSuccess B.empty (convertInputWithRemaining ctx (remaining `B.append` newInput))
                                                     else
                                                         -- Converted the complete remainder. Now let's try to
                                                         -- convert the remaining new input
                                                         convertInput ctx c (B.drop consumedInput newInput)

                                                     where
                                                         processed = B.length tmpInput - B.length r
                                                         consumedInput = processed - B.length remaining
                    InvalidInput              c _ -> if B.null c then
                                                         ConvertInvalidInputError
                                                     else
                                                         -- Converted a bit but detected invalid input afterwards.
                                                         -- Let's return what was converted so far and fail with
                                                         -- the next call
                                                         ConvertSuccess c (const ConvertInvalidInputError)
                    UnexpectedError (Errno errno) -> ConvertUnexpectedConversionError (show errno)


-- Thin wrapper around iconv_open()
data IConvT = IConvT (ForeignPtr IConvT)
data IConvOpenError =
    UnsupportedConversion
  | UnexpectedOpenError Errno

iconvOpen :: CharacterEncoding -> CharacterEncoding -> Either IConvOpenError IConvT
iconvOpen inputEncoding outputEncoding = unsafePerformIO $
    mask_ $ do -- mask async exceptions, we might otherwise leak
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
                    return $ Right (IConvT fPtr)

-- Thin wrapper around iconv()
data IConvResult =
    Converted B.ByteString B.ByteString
  | InvalidInput B.ByteString B.ByteString
  | UnexpectedError Errno

iconv :: IConvT -> B.ByteString -> B.ByteString -> IConvResult
iconv (IConvT fPtr) input outputPrefix = unsafePerformIO $
    mask_ $ do -- mask async exceptions, we might otherwise leak
    let outputPrefixLen = B.length outputPrefix
        inputLen        = B.length input
                          -- We allocate enough memory for the worst case: 1
                          -- byte per character encodings to 4 bytes per
                          -- character encodings (e.g. ASCII to UTF32).
                          -- Additionally 16 bytes of padding just in case and
                          -- the length of the converted prefix we have to
                          -- prepend to the result
                          --
                          -- We overallocate here but will resize later. The
                          -- alternative would be to produce potentially many
                          -- smaller chunks of output.
        outputLen       = inputLen * 4 + 16 + outputPrefixLen
        convertLen      = outputLen - outputPrefixLen

    -- We allocate via malloc(), which does not give us memory inside
    -- the GC heap. But this allows us to realloc() the memory later
    -- to the actual size.
    outputPtr <- mallocBytes outputLen

    BU.unsafeUseAsCString outputPrefix $ \outputPrefixPtr ->
        copyBytes outputPtr outputPrefixPtr outputPrefixLen

    -- Newly converted output should start at this pointer
    let outputConvPtr = plusPtr outputPtr outputPrefixLen

    -- Do the actual conversion and calculate how many bytes we
    -- read and wrote to update our state
    (res, readCount, writeCount) <-
        withForeignPtr fPtr            $ \ptr              ->
        BU.unsafeUseAsCString input    $ \inputPtr         ->
        with inputPtr                  $ \inputPtrPtr      ->
        with (fromIntegral inputLen)   $ \inputLenPtr      ->
        with outputConvPtr             $ \outputConvPtrPtr ->
        with (fromIntegral convertLen) $ \convertLenPtr    -> do
            res        <- c_iconv ptr inputPtrPtr inputLenPtr outputConvPtrPtr convertLenPtr

            -- The length pointers will be updated by iconv to the still
            -- remaining length after conversion. That means we can calculate
            -- the read/written number of bytes with: old - new
            readCount  <- (`subtract` inputLen)   . fromIntegral <$> peek inputLenPtr
            writeCount <- (`subtract` convertLen) . fromIntegral <$> peek convertLenPtr
            return (res, readCount, writeCount)

    let remaining    = B.drop readCount input
        convertedLen = outputPrefixLen + writeCount

    -- Reallocate the memory to a memory area of the actual size. This
    -- potentially allows the OS to reuse any memory we allocated too much and
    -- in general does not cause the memory to be copied.
    --
    -- This will return NULL if the output size is 0, but the resulting
    -- ByteString will then be the empty ByteString as expected.
    --
    -- Shadowing outputPtr to prevent accidential usage of old outputPtr
    outputPtr <- reallocBytes outputPtr convertedLen
    output <- BU.unsafePackMallocCStringLen (outputPtr, convertedLen)

    if res /= (-1) then
        return $ Converted output remaining
    else do
        errno <- getErrno
        case () of
                -- The output buffer was too small! This should not happen
                -- because we overallocate for any possible output charset
                -- encoding
            _ | errno == e2BIG  -> return $ UnexpectedError errno

                -- Incomplete byte sequence was detected, which we might be
                -- able to convert with further input later
              | errno == eINVAL -> return $ Converted output remaining

                -- Invalid byte sequence was detected. We might've converted
                -- something already but from here on we can't do anything
              | errno == eILSEQ -> return $ InvalidInput output remaining

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

