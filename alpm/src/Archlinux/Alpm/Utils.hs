{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Archlinux.Alpm.Utils where

import Control.Exception (Exception)
import Control.Monad (foldM, unless)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable)
import Foreign
import Foreign.C
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, handle)
import Prelude

import Archlinux.Alpm.Binding (AlpmErrNo, AlpmHandlePtr, AlpmListPtr)

import Archlinux.Alpm.Binding qualified as Binding

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data AlpmError a = AlpmError AlpmErrNo a
    deriving (Eq, Show)

instance (Show a, Typeable a) => Exception (AlpmError a)

data AlpmUnknownError = UnknownAlpmError
    deriving (Eq, Show)

instance Exception AlpmUnknownError

errno :: (MonadIO m) => AlpmHandlePtr -> m AlpmErrNo
errno h = toEnum . fromIntegral <$> liftIO (Binding.errno h)

strerror :: (MonadIO m) => AlpmErrNo -> m String
strerror e = liftIO $ do
    xs <- Binding.strerror . fromIntegral . fromEnum $ e
    peekCString xs

throwAlpmError :: (MonadIO m, MonadThrow m) => AlpmHandlePtr -> m CInt -> m ()
throwAlpmError h = throwAlpmError' h UnknownAlpmError

throwAlpmError'
    :: (MonadIO m, MonadThrow m, Show e, Typeable e)
    => AlpmHandlePtr -> e -> m CInt -> m ()
throwAlpmError' h x = throwAlpmErrorM' h (pure x)

throwAlpmErrorM'
    :: (MonadIO m, MonadThrow m, Show e, Typeable e)
    => AlpmHandlePtr -> m e -> m CInt -> m ()
throwAlpmErrorM' h x k = do
    ec <- k
    unless (ec == 0) $
        throwM =<< (AlpmError <$> errno h <*> x)

modifyAlpmErrorM
    :: (MonadThrow m, MonadUnliftIO m, Show e, Show e', Typeable e, Typeable e')
    => (AlpmErrNo -> e -> m e') -> m c -> m c
modifyAlpmErrorM f = handle (\(AlpmError err x) -> f err x >>= throwM . AlpmError err)

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

fromAlpmList :: AlpmListPtr -> IO [Ptr a]
fromAlpmList = traverseAlpmList (pure . (: []))

peekAlpmList :: (Ptr a -> IO a) -> AlpmListPtr -> IO [a]
peekAlpmList peekF = traverseAlpmList $ \p' -> do
    x <- peekF p'
    pure [x]

peekAlpmListFree :: (Ptr a -> IO a) -> (Ptr a -> IO ()) -> AlpmListPtr -> IO [a]
peekAlpmListFree peekF freeF p = freeListAfter p $
    traverseAlpmList $ \p' -> do
        x <- peekF p'
        freeF p'
        pure [x]

peekAlpmStringList :: AlpmListPtr -> IO [String]
peekAlpmStringList = traverseAlpmList $ \p' -> do
    x <- peekCString p'
    pure [x]

peekAlpmStringListFree :: AlpmListPtr -> IO [String]
peekAlpmStringListFree p = do
    res <-
        traverseAlpmList
            ( \p' -> do
                x <- peekCString p'
                free p'
                pure [x]
            )
            p
    Binding.list_free p
    pure res

toAlpmList :: [Ptr a] -> IO AlpmListPtr
toAlpmList = foldM (\memo -> Binding.list_add memo . castPtr) nullPtr

withAlpmList :: [Ptr a] -> (AlpmListPtr -> IO b) -> IO b
withAlpmList xs = bracket (toAlpmList xs) Binding.list_free

freeListAfter :: AlpmListPtr -> (AlpmListPtr -> IO a) -> IO a
freeListAfter p k = do
    res <- k p
    Binding.list_free p
    pure res

traverseAlpmList :: (Monoid m) => (Ptr a -> IO m) -> AlpmListPtr -> IO m
traverseAlpmList f = go id
    where
        go !contF xs
            | xs == nullPtr = pure (contF mempty)
            | otherwise = do
                x <- Binding.get__list_t__data xs
                xs' <- Binding.get__list_t__next xs
                y <- f $ castPtr x
                go (contF . (y <>)) xs'

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings xss f = go [] xss
    where
        go !acc [] = f $ reverse acc
        go !acc (x : xs) = withCString x (\x' -> go (x' : acc) xs)

peekMaybe :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
peekMaybe f p
    | p == nullPtr = return Nothing
    | otherwise = Just <$> f p

pokeMaybe
    :: (MonadIO m, Storable a) => (Ptr a -> a -> m ()) -> Maybe a -> m (Ptr a)
pokeMaybe _ Nothing = return nullPtr
pokeMaybe f (Just x) = do
    p <- liftIO malloc
    f p x
    return p

makeBitmask :: (Enum a) => [a] -> CInt
makeBitmask = foldl' (\memo x -> memo .|. fromIntegral (fromEnum x)) zeroBits
