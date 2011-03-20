module Codec.Zlib.Enum (
    -- * Enumeratees
    -- ** Zlib Format
    compressZlib, decompressZlib,
    -- ** GZip Format
    compressGZip, decompressGZip,
    -- ** Custom Parameters
    compressWith, decompressWith,
    -- * Re-exported from zlib-bindings
    WindowBits, defaultWindowBits
) where

import Codec.Zlib
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.ByteString (ByteString)

------------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------------

zlibConfig :: WindowBits
zlibConfig =  WindowBits 15

gzipConfig :: WindowBits
gzipConfig =  WindowBits 31

------------------------------------------------------------------------------
-- Decompression
------------------------------------------------------------------------------

-- |
-- Decompress a stream of 'ByteString's in the zlib format.
decompressZlib
    :: MonadIO m
    => Enumeratee ByteString ByteString m a
decompressZlib = decompressWith zlibConfig

-- |
-- Decompress a stream of 'ByteString's in the gzip format.
decompressGZip
    :: MonadIO m
    => Enumeratee ByteString ByteString m a
decompressGZip = decompressWith gzipConfig

-- |
-- Decompress (inflate) a stream of 'ByteString's with custom
-- parameters. For example:
--
-- >    run $ enumFile "test.z" $$ decompressWith defaultWindowBits $$ printChunks True

decompressWith
    :: MonadIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m a
decompressWith config inner = do
    inf <- liftIO $ initInflate config
    decompressWith' inf inner

decompressWith'
    :: MonadIO m
    => Inflate
    -> Enumeratee ByteString ByteString m b
decompressWith' inf (Continue k) = do
    x <- EL.head
    case x of
        Nothing -> do
            chunk <- liftIO $ finishInflate inf
            lift $ runIteratee $ k $ Chunks [chunk]
        Just bs -> do
            chunks <- liftIO $ withInflateInput inf bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            decompressWith' inf step
    where
    go front pop = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just y -> go (front . (:) y) pop
decompressWith' _ step = return step


------------------------------------------------------------------------------
-- Compression
------------------------------------------------------------------------------

-- |
-- Compress a stream of 'ByteString's into the zlib format.
compressZlib
    :: MonadIO m
    => Enumeratee ByteString ByteString m a
compressZlib = compressWith 6 zlibConfig
-- Note: Using the same settings as package zlib

-- |
-- Compress a stream of 'ByteString's into the gzip format.
compressGZip
    :: MonadIO m
    => Enumeratee ByteString ByteString m a
compressGZip = compressWith 6 gzipConfig
-- Note: Using the same settings as package zlib

-- |
-- Compress (deflate) a stream of 'ByteString's with custom
-- parameters. The 'WindowBits' also control the format (zlib vs. gzip).

compressWith
    :: MonadIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m a
compressWith level config inner = do
    def <- liftIO $ initDeflate level config
    compressWith' def inner

compressWith'
    :: MonadIO m
    => Deflate
    -> Enumeratee ByteString ByteString m b
compressWith' def (Continue k) = do
    x <- EL.head
    case x of
        Nothing -> do
            chunks <- liftIO $ finishDeflate def $ go id
            lift $ runIteratee $ k $ Chunks chunks
        Just bs -> do
            chunks <- liftIO $ withDeflateInput def bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            compressWith' def step
    where
    go front pop = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just y -> go (front . (:) y) pop
compressWith' _ step = return step


-- testInflate = do
--     h <- openBinaryFile "test-out" WriteMode
--     run $ enumFile "test.z"
--            $$ decompressWith defaultWindowBits
--            $$ iterHandle h
--     hClose h
--
-- testDeflate = do
--     h <- openBinaryFile "test.z" WriteMode
--     run $ enumFile "test"
--            $$ compressWith 7 defaultWindowBits
--            $$ iterHandle h
--     hClose h
