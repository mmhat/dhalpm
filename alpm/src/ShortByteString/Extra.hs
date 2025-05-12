{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ShortByteString.Extra (
    newCString,
    toBuilder,
) where

import Data.ByteString.Short (ShortByteString)
import Data.Text.Builder.Linear (Builder)
import Data.Word (Word8)
import Foreign.C (CString)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (pokeByteOff)
import Prelude

import Data.ByteString.Short qualified as ShortByteString
import Data.ByteString.Short.Internal qualified as ShortByteString
import Data.Text.Builder.Linear qualified
import GHC.Exts qualified

newCString :: ShortByteString -> IO CString
newCString sbs = do
    let
        l = ShortByteString.length sbs
    ptr <- mallocBytes (l + 1)
    ShortByteString.copyToPtr sbs 0 ptr l
    pokeByteOff ptr l (0 :: Word8)
    pure ptr

-- | _WARNING_: We do not check that the 'ShortByteString' has a valid UTF-8
-- encoding!
toBuilder :: ShortByteString -> Builder
toBuilder (ShortByteString.SBS array) =
    GHC.Exts.runRW# $! \s0 ->
        let
            !(# _s3, pinned #) =
                if GHC.Exts.isTrue# (GHC.Exts.isByteArrayPinned# array)
                    then (# s0, array #)
                    else
                        let
                            size = GHC.Exts.sizeofByteArray# array
                            !(# s1, marray #) = GHC.Exts.newPinnedByteArray# size s0
                            s2 = GHC.Exts.copyByteArray# array 0# marray 0# size s1
                        in
                            GHC.Exts.unsafeFreezeByteArray# marray s2
            addr = GHC.Exts.byteArrayContents# pinned
            builder = Data.Text.Builder.Linear.fromAddr addr
        in
            builder
{-# INLINE toBuilder #-}
