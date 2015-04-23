{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.CaboCha (
    new,
    parse,
    parseToChunks,
    Token(..),
    Chunk(..),
    CaboChaString(..)
)

where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throwIO)
import Control.Monad (when, forM)
import qualified Data.ByteString as B (ByteString, packCString, useAsCString)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import Data.Typeable (Typeable)
import Foreign (
    ForeignPtr, FunPtr, Ptr,
    newForeignPtr, nullPtr, peek, peekByteOff, withArrayLen, withForeignPtr)
import Foreign.C (
    CChar(..), CFloat(..), CInt(..), CSize(..),
    CString, peekCString, withCString)
import Foreign.Storable (sizeOf)
import Foreign.Ptr (plusPtr)

#include <cabocha.h>

newtype CaboCha =
    CaboCha { pCaboCha :: ForeignPtr CaboCha }
    deriving (Eq, Ord)

data Tree

data CaboChaError =
    CaboChaError String
    deriving (Eq, Ord, Show, Typeable)

instance Exception CaboChaError

class CaboChaString s where
    toByteString :: s -> B.ByteString
    fromByteString :: B.ByteString -> s

instance CaboChaString String where
    toByteString = toByteString . T.pack
    fromByteString = T.unpack . fromByteString

instance CaboChaString B.ByteString where
    toByteString = id
    fromByteString = id

instance CaboChaString T.Text where
    toByteString = T.encodeUtf8
    fromByteString = T.decodeUtf8

data Chunk a =
    Chunk {
        chunkLink :: Int,
        chunkHeadPos :: Int,
        chunkFuncPos :: Int,
        chunkTokenPos :: Int,
        chunkTokenSize :: Int,
        chunkScore :: Double,
        chunkAdditionalInfo :: Maybe a,
        chunkTokens :: [Token a]
    } deriving (Eq, Read, Show)

data Token a =
    Token {
        tokenSurface :: a,
        tokenNormalizedSurface :: a,
        tokenFeatureList :: [a],
        tokenNE :: Maybe a,
        tokenAdditionalInfo :: Maybe a
    } deriving (Eq, Read, Show)

new :: [String] -> IO CaboCha
new args =
    withCStrings args $ \argc argv -> do
        pcabocha <- cabocha_new (fromIntegral argc) argv
        when (pcabocha == nullPtr) $
            throwIO =<< (CaboChaError <$> strerror nullPtr)
        CaboCha <$> newForeignPtr p_cabocha_destroy pcabocha

strerror :: Ptr CaboCha -> IO String
strerror pcabocha = peekCString =<< cabocha_strerror pcabocha

parse :: CaboChaString a => CaboCha -> a -> IO a
parse cabocha s =
    withForeignPtr (pCaboCha cabocha) $ \pc ->
        B.useAsCString (toByteString s) $ \pstr -> do
            presult <- cabocha_sparse_tostr pc pstr
            when (presult == nullPtr) $
                throwIO =<< (CaboChaError <$> strerror pc)
            packCString presult

parseToChunks :: CaboChaString a => CaboCha -> a -> IO [Chunk a]
parseToChunks cabocha s =
    withForeignPtr (pCaboCha cabocha) $ \pc ->
        B.useAsCString (toByteString s) $ \pstr -> do
            ptree <- cabocha_sparse_totree pc pstr
            when (ptree == nullPtr) $
                throwIO =<< (CaboChaError <$> strerror pc)
            n <- cabocha_tree_token_size ptree
            ts <- forM [0 .. (n - 1)] $ \i ->
                peekToken =<< cabocha_tree_token ptree i
            n' <- cabocha_tree_chunk_size ptree
            cs <- forM [0 .. (n' - 1)] $ \i -> do
                pchunk <- cabocha_tree_chunk ptree i
                peekChunk pchunk ts
            cabocha_tree_clear ptree
            return cs

withCStrings :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStrings ss f =
    withCStrings' ss $
        \pstrs -> withArrayLen pstrs f

withCStrings' :: [String] -> ([CString] -> IO a) -> IO a
withCStrings' ss f = go [] ss
    where
        go acc [] = f $ reverse acc
        go acc (s : rest) = withCString s $ \x -> go (x : acc) rest

peekChunk :: CaboChaString a => Ptr (Chunk a) -> [Token a] -> IO (Chunk a)
peekChunk pchunk tokens = do
    i <- (fromIntegral :: CSize -> Int) <$> (#peek cabocha_chunk_t, token_pos) pchunk
    n <- (fromIntegral :: CSize -> Int) <$> (#peek cabocha_chunk_t, token_size) pchunk
    Chunk
        <$> ((fromIntegral :: CInt -> Int) <$> (#peek cabocha_chunk_t, link) pchunk)
        <*> ((fromIntegral :: CSize -> Int) <$> (#peek cabocha_chunk_t, head_pos) pchunk)
        <*> ((fromIntegral :: CSize -> Int) <$> (#peek cabocha_chunk_t, func_pos) pchunk)
        <*> return i
        <*> return n
        <*> ((realToFrac :: CFloat -> Double) <$> (#peek cabocha_chunk_t, score) pchunk)
        <*> (packOptionalCString =<< (#peek cabocha_chunk_t, additional_info) pchunk)
        <*> return ((take n . drop i) tokens)

peekToken :: CaboChaString a => Ptr (Token a) -> IO (Token a)
peekToken ptoken =
    Token
        <$> (packCString =<< (#peek cabocha_token_t, surface) ptoken)
        <*> (packCString =<< (#peek cabocha_token_t, normalized_surface) ptoken)
        <*> (packCStrings =<< (#peek cabocha_token_t, feature_list) ptoken)
        <*> (packOptionalCString =<< (#peek cabocha_token_t, ne) ptoken)
        <*> (packOptionalCString =<< (#peek cabocha_token_t, additional_info) ptoken)

packCString :: CaboChaString a => CString -> IO a
packCString cstr = fromByteString <$> B.packCString cstr

packCStrings :: CaboChaString b => Ptr (Ptr CChar) -> IO [b]
packCStrings pstrs =
    peek pstrs >>= \pstr ->
        if (pstr == nullPtr)
        then return []
        else do
            let size = sizeOf (undefined :: Ptr CChar)
            s <- packCString pstr
            ss <- packCStrings (pstrs `plusPtr` size)
            return (s : ss)

packOptionalCString :: CaboChaString a => CString -> IO (Maybe a)
packOptionalCString pstr =
    if pstr == nullPtr
    then return Nothing
    else Just <$> packCString pstr

foreign import ccall "cabocha_new"
    cabocha_new :: CInt -> Ptr CString -> IO (Ptr CaboCha)

foreign import ccall "cabocha_strerror"
    cabocha_strerror :: Ptr CaboCha -> IO CString

foreign import ccall "cabocha_sparse_tostr"
    cabocha_sparse_tostr :: Ptr CaboCha -> CString -> IO CString

foreign import ccall "&cabocha_destroy"
    p_cabocha_destroy :: FunPtr (Ptr CaboCha -> IO ())

foreign import ccall "cabocha_sparse_totree"
    cabocha_sparse_totree :: Ptr CaboCha -> CString -> IO (Ptr Tree)

foreign import ccall "cabocha_tree_chunk"
    cabocha_tree_chunk :: Ptr Tree -> CSize -> IO (Ptr (Chunk a))

foreign import ccall "cabocha_tree_chunk_size"
    cabocha_tree_chunk_size :: Ptr Tree -> IO CSize

foreign import ccall "cabocha_tree_token"
    cabocha_tree_token :: Ptr Tree -> CSize -> IO (Ptr (Token a))

foreign import ccall "cabocha_tree_token_size"
    cabocha_tree_token_size :: Ptr Tree -> IO CSize

foreign import ccall "cabocha_tree_clear"
    cabocha_tree_clear :: Ptr Tree -> IO ()
