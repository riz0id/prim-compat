{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Word.Prim.Compat
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- MaWordainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- "Data.Word.Prim.Compat" is a backwards compatibility layer over "GHC.Prim"
-- re-exporting types and functions related to the unboxed unsigned integer 
-- types 'Word#', 'Word8#', 'Word16#', and 'Word32#'.
--
-- @since 1.0.0
module Data.Word.Prim.Compat 
  ( -- * Word#
    Word#,
    fromWord#,
    toWord#,

    -- * Word8#
    Word8#,
    fromWord8#,
    toWord8#,

    -- * Word16#
    Word16#,
    fromWord16#,
    toWord16#,

    -- * Word32#
    Word32#,
    fromWord32#,
    toWord32#,
  )
where

import GHC.Word (Word (W#), Word16 (W16#), Word32 (W32#), Word8 (W8#))
import GHC.Exts (Word#, Word16#, Word32#, Word8#)

#if !(MIN_VERSION_ghc_prim(0,8,0))

import qualified GHC.Exts as GHC

#endif

-- Word# -----------------------------------------------------------------------

fromWord# :: Word# -> Word
fromWord# = W# 

toWord# :: Word -> Word#
toWord# (W# x) = x

-- Word8# ----------------------------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

fromWord8# :: Word8# -> Word8
fromWord8# = W8#

toWord8# :: Word8 -> Word8#
toWord8# (W8# x) = x

#else 

fromWord8# :: Word8# -> Word8
fromWord8# x = W8# (GHC.extendWord8# x)

toWord8# :: Word8 -> Word8#
toWord8# (W8# x) = GHC.narrowWord8# x

#endif

-- Word16# ---------------------------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

fromWord16# :: Word16# -> Word16
fromWord16# = W16#

toWord16# :: Word16 -> Word16#
toWord16# (W16# x) = x

#else 

fromWord16# :: Word16# -> Word16
fromWord16# x = W16# (GHC.extendWord16# x)

toWord16# :: Word16 -> Word16#
toWord16# (W16# x) = GHC.narrowWord16# x

#endif

-- Word32# ---------------------------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

fromWord32# :: Word32# -> Word32
fromWord32# = W32#

toWord32# :: Word32 -> Word32#
toWord32# (W32# x) = x

#else 

fromWord32# :: Word32# -> Word32
fromWord32# x = W32# (GHC.unsafeCoerce# x)

toWord32# :: Word32 -> Word32#
toWord32# (W32# x) = GHC.unsafeCoerce# x

#endif