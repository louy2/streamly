-- |
-- Module      : Streamly.Internal.Data.Json.Stream
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedLists #-}

module Streamly.Internal.Data.Json.Stream where

import Control.Monad.Catch (MonadCatch)
import Data.Char (ord)
import Data.Word (Word8)
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S

import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array (Array)
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Data.Fold as FL

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

backslash = 92 :: Word8
close_curly = 125 :: Word8
close_square = 93 :: Word8
comma = 44 :: Word8
double_quote = 34 :: Word8
open_curly = 123 :: Word8
open_square = 91 :: Word8
c_0 = 48 :: Word8
c_9 = 57 :: Word8
c_A = 65 :: Word8
c_F = 70 :: Word8
c_a = 97 :: Word8
c_f = 102 :: Word8
c_n = 110 :: Word8
c_t = 116 :: Word8

-- | A JSON \"object\" (key\/value map).
type Object = HashMap (Array Char) Value

-- | A JSON value represented as a Haskell value.
data Value
    = Object !Object
    | Array !(Array Value)
    | String !(Array Char)
    | Number !Scientific
    | Bool !Bool
    | Null
    deriving (Eq, Show)

skipSpace :: MonadCatch m => Parser m Word8 ()
skipSpace =
    P.takeWhile
        (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)
        FL.drain

skip :: MonadCatch m => Int -> Parser m a ()
skip n = P.take n FL.drain

string :: MonadCatch m => String -> Parser m Word8 ()
string = P.eqBy (==) . map (fromIntegral . ord)

parseJson :: MonadCatch m => Parser m Word8 Value
parseJson = do
    skipSpace
    w <- P.peek
    case w of
        OPEN_CURLY -> do
            skip 1
            return Null
        C_f -> do
            string "false"
            return $ Bool False
        C_t -> do
            string "true"
            return $ Bool True
        C_n -> do
            string "null"
            return Null
