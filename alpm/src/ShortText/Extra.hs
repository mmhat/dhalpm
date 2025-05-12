{-# OPTIONS_GHC -Wno-orphans #-}

module ShortText.Extra (
    newCString,
    unsafePackCString,
    unsafePackCStringLen,
) where

import Data.Text.Display (Display (..))
import Data.Text.Short (ShortText)
import Foreign.C (CString, CStringLen)
import Prelude

import Data.ByteString.Short qualified as ShortByteString
import Data.Text.Short qualified as ShortText
import Data.Text.Short.Unsafe qualified as ShortText

import ShortByteString.Extra qualified

newCString :: ShortText -> IO CString
newCString = ShortByteString.Extra.newCString . ShortText.toShortByteString

unsafePackCString :: CString -> IO ShortText
unsafePackCString =
    fmap ShortText.fromShortByteStringUnsafe
        . ShortByteString.packCString

unsafePackCStringLen :: CStringLen -> IO ShortText
unsafePackCStringLen =
    fmap ShortText.fromShortByteStringUnsafe
        . ShortByteString.packCStringLen

--------------------------------------------------------------------------------
-- Orphans
--------------------------------------------------------------------------------

instance Display ShortText where
    displayBuilder =
        ShortByteString.Extra.toBuilder
            . ShortText.toShortByteString
    {-# INLINE displayBuilder #-}
