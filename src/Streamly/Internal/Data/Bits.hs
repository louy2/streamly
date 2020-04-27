{-# LANGUAGE CPP, MagicHash #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Unsafe #-}
#endif
-- |
-- Copyright   : (c) 2010 Simon Meier
--
--               Original serialization code from 'Data.Binary.Builder':
--               (c) Lennart Kolmodin, Ross Patterson
--
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Unchecked bit shifts.
--
-- These functions are undefined when the amount being shifted by is
-- greater than the size in bits of a machine Int#.-
--
#if !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Streamly.Internal.Data.Bits
    ( shiftr_w16
    , shiftr_w32
    , shiftr_w64
    , shiftr_w

    , shiftl_w16
    , shiftl_w32
    , shiftl_w64
    , shiftl_w

    , caseWordSize_32_64
    ) where


#if !defined(__HADDOCK__)
import GHC.Base
import GHC.Word (Word32(..),Word16(..),Word64(..))

#if WORD_SIZE_IN_BITS < 64 && __GLASGOW_HASKELL__ >= 608
import GHC.Word (uncheckedShiftRL64#)
#endif
#else
import Data.Word
#endif

import Foreign

-------------------------------------------------------------------------------
-- Unchecked right shifts
-------------------------------------------------------------------------------

-- | Right-shift of a 'Word16'.
{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16

-- | Right-shift of a 'Word32'.
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32

-- | Right-shift of a 'Word64'.
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

#if !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)
#else
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif

-- | Right-shift of a 'Word'.
{-# INLINE shiftr_w #-}
shiftr_w :: Word -> Int -> Word
#if WORD_SIZE_IN_BITS < 64
shiftr_w w s = fromIntegral $ (`shiftr_w32` s) $ fromIntegral w
#else
shiftr_w w s = fromIntegral $ (`shiftr_w64` s) $ fromIntegral w
#endif

-------------------------------------------------------------------------------
-- Unchecked left shifts
-------------------------------------------------------------------------------

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#` i)

shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#` i)

#if WORD_SIZE_IN_BITS < 64
#define ushiftl_64 uncheckedShiftL64#
#else
#define ushiftl_64 uncheckedShiftL#
#endif

shiftl_w64 :: Word64 -> Int -> Word64
shiftl_w64 (W64# w) (I# i) = W64# (w `ushiftl_64` i)

-- | Left-shift of a 'Word'.
{-# INLINE shiftl_w #-}
shiftl_w :: Word -> Int -> Word
#if WORD_SIZE_IN_BITS < 64
shiftl_w w s = fromIntegral $ (`shiftl_w32` s) $ fromIntegral w
#else
shiftl_w w s = fromIntegral $ (`shiftl_w64` s) $ fromIntegral w
#endif

-------------------------------------------------------------------------------
-- Select 32/64 bit impl
-------------------------------------------------------------------------------

-- | Select an implementation depending on the bit-size of 'Word's.
-- Currently, it produces a runtime failure if the bitsize is different.
-- This is detected by the testsuite.
{-# INLINE caseWordSize_32_64 #-}
caseWordSize_32_64 :: a -- Value to use for 32-bit 'Word's
                   -> a -- Value to use for 64-bit 'Word's
                   -> a
caseWordSize_32_64 f32 f64 =
#if MIN_VERSION_base(4,7,0)
  case finiteBitSize (undefined :: Word) of
#else
  case bitSize (undefined :: Word) of
#endif
    32 -> f32
    64 -> f64
    s  -> error $ "caseWordSize_32_64: unsupported Word bit-size " ++ show s
