{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.FileSystem.IOVec
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low level IO routines interfacing the operating system.
--

module Streamly.FileSystem.IOVec
    ( IOVec(..)
    , c_writev
    , c_safe_writev
    )
where

import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.Posix.Types (CSsize(..))

-------------------------------------------------------------------------------
-- IOVec
-------------------------------------------------------------------------------

#if !defined(mingw32_HOST_OS)

#include <sys/uio.h>

data IOVec = IOVec
  { iovBase :: {-# UNPACK #-} !(Ptr Word8)
  , iovLen  :: {-# UNPACK #-} !Word64
  } deriving (Eq, Show)

instance Storable IOVec where
  sizeOf _ = #{size struct iovec}
  alignment _ = #{alignment struct iovec}
  peek ptr = do
      base <- #{peek struct iovec, iov_base} ptr
      len  :: #{type size_t} <- #{peek struct iovec, iov_len}  ptr
      return $ IOVec base len
  poke ptr vec = do
      let base = iovBase vec
          len  :: #{type size_t} = iovLen  vec
      #{poke struct iovec, iov_base} ptr base
      #{poke struct iovec, iov_len}  ptr len

foreign import capi unsafe "sys/uio.h writev"
   c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import capi safe "sys/uio.h writev"
   c_safe_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

#else
c_writev = error "writev not implemented for windows"
c_safe_writev = error "writev not implemented for windows"
#endif