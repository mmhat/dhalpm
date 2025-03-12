{-# LANGUAGE RecordWildCards #-}

module Archlinux.Alpm.Names where

import Prelude


import Control.Applicative (many, optional)
import Control.Exception (Exception (..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Bytes.Parser (Parser)
import Data.ByteString.Short (ShortByteString)
import Data.Coerce (coerce)
--import Data.String (IsString (..))
--import Data.Text.Display (Display (..))
import Foreign hiding (void)
import Foreign.C

import Data.Bytes.Builder qualified as Builder
import Data.Bytes.Chunks qualified as Bytes
import Data.Bytes.Parser qualified as Parser
import Data.Bytes.Parser.Ascii qualified as Parser
import Data.ByteString.Short qualified as ShortByteString
import Data.ByteString.Short.Internal qualified as ShortByteString.Internal
import System.IO.Unsafe qualified as Unsafe

#include <alpm.h>

{#context prefix = "alpm" add prefix = "Alpm" #}

newtype DatabaseName = DatabaseName { unDatabaseName :: ShortByteString }
    deriving (Eq, Ord, Show)

newtype GroupName = GroupName { unGroupName :: ShortByteString }
    deriving (Eq, Ord, Show)

newtype HookName = HookName { unHookName :: ShortByteString }
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Package names
--------------------------------------------------------------------------------

newtype PackageName = PackageName { unPackageName :: ShortByteString }
    deriving (Eq, Ord, Show)

--instance Display PackageName where
--    displayBuilder = displayBuilder . Text.pack . unAlpmPkgName

--instance IsString PackageName where
--    fromString = either (error . displayException) id . parseAlpmPkgName . Text.pack

emptyPackageName :: PackageName
emptyPackageName = PackageName ShortByteString.empty

parsePackageName :: MonadThrow m => ShortByteString -> m PackageName
parsePackageName sbs =
    case Parser.parseByteArray (packageNameParser <* Parser.endOfInput ["end of input"]) (coerce sbs) of
        Parser.Failure e  -> throwM (PackageNameParseException sbs e)
        Parser.Success (Parser.Slice _ _ name) -> pure name

packageNameParser :: Parser [String] s PackageName
packageNameParser =
    PackageName . ShortByteString.pack
        <$> ( (:)
                <$> Parser.satisfy ["invalid first character"] isValidChar0
                <*> many (Parser.satisfy ["invalid character"] isValidChar)
            )
    where
        isValidChar0 :: Word8 -> Bool
        isValidChar0 w =
            (48 <= w && w <= 57) -- "0123456789"
                || (65 <= w && w <= 90) -- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                || (97 <= w && w <= 122) -- "abcdefghijklmnopqrstuvwxyz"
                || w == 64 -- '@'
                || w == 95 -- '_'
                || w == 43 -- '+'

        isValidChar :: Word8 -> Bool
        isValidChar w =
            isValidChar0 w
                || w == 46 -- '.'
                || w == 45 -- '-'

data PackageNameParseException = PackageNameParseException ShortByteString [String]
    deriving (Eq, Show)

instance Exception PackageNameParseException

--------------------------------------------------------------------------------
-- Package versions
--------------------------------------------------------------------------------

data Version = Version
    { versionEpoch :: Maybe Word
    , versionVer :: ShortByteString
    , versionRel :: Word
    , versionSubRel :: Maybe Word
    } deriving (Show)

instance Eq Version where
    x == y = compare x y == EQ

instance Ord Version where
    x `compare` y = Unsafe.unsafePerformIO (vercmp (showVersion x) (showVersion y))

newtype VersionString = VersionString { unVersionString :: ShortByteString }

parseVersion :: MonadThrow m => ShortByteString -> m Version
parseVersion sbs =
    case Parser.parseByteArray (versionParser <* Parser.endOfInput undefined) (coerce sbs) of
        Parser.Failure e  -> throwM (VersionParseException sbs e)
        Parser.Success (Parser.Slice _ _ version) -> pure version

versionParser :: Parser [String] s Version
versionParser = Version
    <$> optional (Parser.decWord ["epoch: not a number"] <* Parser.char ["missing colon"] ':')
    <*> (fmap ShortByteString.pack . many)
        (Parser.satisfy ["pkgver: invalid character"] isValidVerChar)
    <* Parser.char ["missing hyphen"] '-'
    <*> Parser.decWord ["pkgrel: not a number"]
    <*> optional (Parser.char ["missing dot"] '.' *> Parser.decWord ["subrel: not a number"])
    where
        isValidVerChar :: Word8 -> Bool
        isValidVerChar w =
            w /= 58 -- ':'
                && w /= 47 -- '/'
                && w /= 45 -- '-'
                && w /= 32 -- ' '

showVersion :: Version -> VersionString
showVersion x = VersionString
    . ShortByteString.Internal.ShortByteString
    . Bytes.concatPinnedU
    . Builder.run 4080
    $ epoch <> ver <> "-" <> rel <> subrel
    where
        epoch = maybe mempty ((<> ":") . Builder.wordDec) (versionEpoch x)
        ver = Builder.shortByteString (versionVer x)
        rel = Builder.wordDec (versionRel x)
        subrel = maybe mempty (("." <>) . Builder.wordDec) (versionSubRel x)

vercmp :: VersionString -> VersionString -> IO Ordering
vercmp (VersionString xs) (VersionString ys) =
    ShortByteString.useAsCString xs $ \xs' ->
    ShortByteString.useAsCString ys $ \ys' ->
        (`compare` 0) <$> {#call alpm_pkg_vercmp #} xs' ys'

data VersionParseException = VersionParseException ShortByteString [String]
    deriving (Eq, Show)

instance Exception VersionParseException
