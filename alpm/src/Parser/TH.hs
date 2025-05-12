{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.TH (
    cchar,
) where

import Foreign.C (CChar (..))
import GHC.Exts (Int#)
import GHC.Int (Int8 (..))
import Language.Haskell.TH.Quote (QuasiQuoter)
import Prelude

import Data.Char qualified
import GHC.Exts qualified
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH

cchar :: QuasiQuoter
cchar =
    TH.QuasiQuoter
        { TH.quoteExp = \string -> do
            let
                !c = unsafeStringToCChar string
            pure (TH.ConE 'CCharQ `TH.AppE` TH.LitE (TH.IntPrimL c))
        , TH.quotePat = \string -> do
            let
                !c = unsafeStringToCChar string
            pure (TH.ConP 'CCharQ [] [TH.LitP (TH.IntPrimL c)])
        , TH.quoteType = const (error "Quoting in type context is not supported")
        , TH.quoteDec = const (error "Quoting in declaration context is not supported")
        }
    where
        unsafeStringToCChar :: String -> Integer
        unsafeStringToCChar [c] = unsafeCharToCChar c
        unsafeStringToCChar _ = error "cchar: Expected exactly one character"

        unsafeCharToCChar :: Char -> Integer
        unsafeCharToCChar char
            | let
                i = Data.Char.ord char
            , min8 <= i
            , i <= max8 =
                fromIntegral i
            | otherwise = error ("cchar: Cannot convert " <> show char <> " to a CChar")

        min8, max8 :: Int
        min8 = fromIntegral (minBound :: Int8)
        max8 = fromIntegral (maxBound :: Int8)

pattern CCharQ :: Int# -> CChar
pattern CCharQ i <- CChar (I8# (GHC.Exts.int8ToInt# -> i))
    where
        CCharQ i = CChar (I8# (GHC.Exts.intToInt8# i))
