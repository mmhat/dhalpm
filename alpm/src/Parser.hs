{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnboxedTuples #-}

module Parser where

import Control.Monad
import Control.Monad.Trans.RWS.Strict (RWST)
import Data.Array.Byte (ByteArray (..))
import Foreign.C (CChar (..), CString, CStringLen)
import GHC.Exts (
    ByteArray#,
    Char (..),
    Int#,
    Int8#,
    RealWorld,
    State#,
    Word8#,
    (+#),
    (>=#),
 )
import GHC.IO (IO (..))
import GHC.Int (Int8 (..))
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Exp, Lift, Q)
import Numeric.Natural (Natural)
import UnliftIO (IORef, liftIO, newIORef, readIORef, writeIORef)
import UnliftIO.Exception (Exception (..), throwIO)
import Prelude

import Control.Monad.Trans.RWS.Strict qualified as RWS
import Data.Char qualified
import Foreign qualified
import Foreign.C qualified
import GHC.Exts qualified
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import Parser.TH

type Parser = RWST CString () CString IO

type Saved = IORef CString

data ParserError
    = UnexpectedEndOfInput Int String
    | UnexpectedCharacter Int String Char
    deriving (Eq, Show)

instance Exception ParserError

runParser :: Parser a -> CString -> IO a
runParser parser start = fst <$> RWS.evalRWST parser start start

runParserString :: Parser a -> String -> IO a
runParserString parser string = Foreign.C.withCString string (runParser parser)

quoter :: (Lift a) => Parser a -> QuasiQuoter
quoter parser =
    TH.QuasiQuoter
        { TH.quoteExp = TH.lift <=< TH.runIO . runParserString parser
        , TH.quotePat = const (error "Quoting in pattern context is not supported")
        , TH.quoteType = const (error "Quoting in type context is not supported")
        , TH.quoteDec = const (error "Quoting in declaration context is not supported")
        }

expected :: String -> Parser a
expected what = do
    start <- Foreign.ptrToIntPtr <$> RWS.ask
    current <- Foreign.ptrToIntPtr <$> RWS.get
    let
        Foreign.IntPtr offset = current - start
    peek
        >>= throwIO . \case
            CChar 0 -> UnexpectedEndOfInput offset what
            char ->
                let
                    char' = (Data.Char.chr . fromIntegral) char
                in
                    UnexpectedCharacter offset what char'

peek :: Parser CChar
peek = liftIO . Foreign.peek =<< RWS.get

satisfies :: (CChar -> Bool) -> Parser Bool
satisfies predicate = predicate <$> peek

is :: CChar -> Parser Bool
is test = satisfies (== test)

isEndOfInput :: Parser Bool
isEndOfInput = is (CChar 0)

check :: String -> (CChar -> Bool) -> Parser ()
check what predicate =
    satisfies predicate >>= \case
        True -> pure ()
        False -> expected what

assert :: String -> CChar -> Parser ()
assert what test = check what (== test)

endOfInput :: Parser ()
endOfInput = assert "end of input" (CChar 0)

advance :: Parser ()
advance = RWS.modify (`Foreign.plusPtr` 1)

advanceWhile :: (CChar -> Bool) -> Parser ()
advanceWhile predicate = do
    char <- peek
    when (predicate char) $ do
        advance
        advanceWhile predicate

save :: Parser Saved
save = newIORef =<< RWS.get

update :: Saved -> Parser ()
update ref = writeIORef ref =<< RWS.get

getSubstring :: Saved -> Parser CStringLen
getSubstring ref = do
    start <- readIORef ref
    current <- Foreign.ptrToIntPtr <$> RWS.get
    let
        Foreign.IntPtr len = current - Foreign.ptrToIntPtr start
    pure (start, len)

number :: Parser (Maybe Natural)
number = do
    char <- peek
    case toDigit char of
        Nothing -> pure Nothing
        Just n0 -> do
            advance
            go n0
    where
        go :: Natural -> Parser (Maybe Natural)
        go !memo = do
            char <- peek
            case toDigit char of
                Nothing -> pure (Just memo)
                Just n -> do
                    advance
                    go (10 * memo + n)

        toDigit :: CChar -> Maybe Natural
        toDigit char
            | [cchar|0|] <= char
            , char <= [cchar|9|] =
                Just (fromIntegral (char - [cchar|0|]))
            | otherwise = Nothing

-- module Parsing.DispositionTable (
--     DispositionTable,
--     Disposition (..),
--     makeDispositionTable,
--     checkString,
--     checkText,
--     checkShortText,
--     checkByteString,
--     checkShortByteString,
--     checkCString,
--     checkCStringLen,
--     CheckError (..),
-- ) where
--
-- import Data.Array.Byte (ByteArray (..))
-- import Data.ByteString (ByteString)
-- import Data.ByteString.Short (ShortByteString)
-- import Data.Text (Text)
-- import Data.Text.Short (ShortText)
-- import Foreign.C (CString, CStringLen)
-- import GHC.Exts (
--     ByteArray#,
--     Char (..),
--     Int (..),
--     Int#,
--     Ptr (..),
--     RealWorld,
--     State#,
--     Word8#,
--     (+#),
--     (<#),
--  )
-- import GHC.IO (IO (..))
-- import Language.Haskell.TH (Exp, Q)
-- import Language.Haskell.TH.Syntax (Lift (..))
-- import System.IO.Unsafe (unsafeDupablePerformIO)
-- import Prelude
--
-- import Data.ByteString qualified as ByteString
-- import Data.ByteString.Short qualified as ShortByteString
-- import Data.Text.Foreign qualified
-- import Data.Text.Short qualified as ShortText
-- import Foreign.C qualified
-- import GHC.Exts qualified
-- import Language.Haskell.TH.Syntax qualified as TH
--
-- import Parsing.Error
--
-- import ShortText.Extra qualified as ShortText

type CChar# = Int8#

pattern EndOfInput# :: CChar#
pattern EndOfInput# = 0#Int8

type Bool# = Int#

pattern False# :: Bool#
pattern False# = 0#

pattern True# :: Bool#
pattern True# = 1#

{-# COMPLETE False#, True# #-}

newtype DispositionTable = DispositionTable ByteArray
    deriving (Lift)

type DispositionTable# = ByteArray#

data Disposition
    = IllegalCharacter
    | ValidCharacter
    | ValidFirstCharacter

type Disposition# = Word8#

pattern IllegalCharacter# :: Disposition#
pattern IllegalCharacter# = 0#Word8

pattern ValidCharacter# :: Disposition#
pattern ValidCharacter# = 1#Word8

pattern ValidFirstCharacter# :: Disposition#
pattern ValidFirstCharacter# = 2#Word8

dispositionToDisposition# :: Disposition -> Disposition#
dispositionToDisposition# IllegalCharacter = IllegalCharacter#
dispositionToDisposition# ValidCharacter = ValidCharacter#
dispositionToDisposition# ValidFirstCharacter = ValidFirstCharacter#

makeDispositionTable :: (Char -> Disposition) -> Q Exp
makeDispositionTable classify = do
    table <- TH.runIO $ IO $ \s0 ->
        let
            !(# s1, marray #) = GHC.Exts.newByteArray# 128# s0

            go :: State# RealWorld -> Int# -> State# RealWorld
            go s i
                | GHC.Exts.isTrue# (i >=# GHC.Exts.int8ToInt# max8) = s
                | otherwise =
                    let
                        c = C# (GHC.Exts.chr# i)
                        w8 = dispositionToDisposition# (classify c)
                        s' = GHC.Exts.writeWord8Array# marray i w8 s
                    in
                        go s' (i +# 1#)

            s2 = go s1 0#

            !(# s3, table' #) = GHC.Exts.unsafeFreezeByteArray# marray s2
        in
            (# s3, DispositionTable (ByteArray table') #)

    TH.lift table
    where
        max8 :: Int8#
        !(CChar (I8# max8)) = maxBound :: CChar

dispositionForCChar# :: DispositionTable# -> CChar# -> Disposition#
dispositionForCChar# table c =
    GHC.Exts.indexWord8Array# table (GHC.Exts.int8ToInt# c)

isValidCharacter# :: DispositionTable# -> CChar# -> Bool#
isValidCharacter# table c =
    GHC.Exts.geWord8# (dispositionForCChar# table c) ValidCharacter#

isValidCharacter :: DispositionTable -> CChar -> Bool
isValidCharacter (DispositionTable (ByteArray table)) (CChar (I8# char)) =
    GHC.Exts.isTrue# (isValidCharacter# table char)

isValidFirstCharacter# :: DispositionTable# -> CChar# -> Bool#
isValidFirstCharacter# table c =
    GHC.Exts.eqWord8# (dispositionForCChar# table c) ValidFirstCharacter#

isValidFirstCharacter :: DispositionTable -> CChar -> Bool
isValidFirstCharacter (DispositionTable (ByteArray table)) (CChar (I8# char)) =
    GHC.Exts.isTrue# (isValidFirstCharacter# table char)
