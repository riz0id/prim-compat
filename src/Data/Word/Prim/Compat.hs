{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Word.Prim.Compat
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- "Data.Word.Prim.Compat" is a backwards compatibility layer over "GHC.Prim"
-- re-exporting types and functions related to the unboxed unsigned integer
-- types 'Word#', 'Word8#', 'Word16#', and 'Word32#'.
--
-- @since 1.0.0
module Data.Word.Prim.Compat (
  -- * Word# #word#
  Word#,
  fromWord#,
  toWord#,

  -- * Word8# #word8#
  Word8#,
  fromWord8#,
  toWord8#,

  -- ** Conversion #word8-conversion#
  wordToWord8#,
  word8ToWord#,

  -- * Word16# #word16#
  Word16#,
  fromWord16#,
  toWord16#,

  -- ** Conversion #word16-conversion#
  wordToWord16#,
  word16ToWord#,

  -- ** Unboxing

  -- * Word32# #word32#
  Word32#,
  fromWord32#,
  toWord32#,

  -- ** Conversion #word32-conversion#
  wordToWord32#,
  word32ToWord#,

  -- ** Arithmetic #word32-arithmetic#
  plusWord32#,
  subWord32#,
  timesWord32#,

  -- ** Comparison #word32-comparison#
  gtWord32#,
  geWord32#,
  eqWord32#,
  neWord32#,
  ltWord32#,
  leWord32#,
) where

import GHC.Exts (Word#, Word16#, Word32#, Word8#)
import qualified GHC.Exts as GHC
import GHC.Word (Word (W#), Word16 (W16#), Word32 (W32#), Word8 (W8#))

#if (MIN_VERSION_ghc_prim(0,8,0))

import GHC.Exts (gtWord32#, geWord32#, eqWord32#, neWord32#, ltWord32#, leWord32#)

#else 

import GHC.Exts (Int#)

#endif

{-# RULES
"Word# -> Word -> Word#" forall x.
  toWord# (fromWord# x) =
    x
"Word8# -> Word8 -> Word8#" forall x.
  toWord8# (fromWord8# x) =
    x
"Word16# -> Word16 -> Word16#" forall x.
  toWord16# (fromWord16# x) =
    x
"Word32# -> Word32 -> Word32#" forall x.
  toWord32# (fromWord32# x) =
    x
  #-}

-- Word# -----------------------------------------------------------------------

-- | Convert a boxed 'Word' value to an unboxed 'Word#' value.
--
-- @since 1.0.0
toWord# :: Word -> Word#
toWord# (W# x) = x
{-# INLINE [0] toWord# #-}

-- | Convert a unboxed 'Word#' value to a boxed 'Word' value.
--
-- @since 1.0.0
fromWord# :: Word# -> Word
fromWord# = W#
{-# INLINE [0] fromWord# #-}

-- Word8# ----------------------------------------------------------------------

-- | Convert a unboxed 'Word8#' value to a boxed 'Word8' value.
--
-- @since 1.0.0
fromWord8# :: Word8# -> Word8
#if (MIN_VERSION_ghc_prim(0,8,0))
fromWord8# = W8#
#else 
fromWord8# x = W8# (GHC.extendWord8# x)
#endif
{-# INLINE [0] toWord8# #-}

-- | Convert a boxed 'Word8' value to an unboxed 'Word8#' value.
--
-- @since 1.0.0
toWord8# :: Word8 -> Word8#
#if (MIN_VERSION_ghc_prim(0,8,0))
toWord8# (W8# x) = x
#else 
toWord8# (W8# x) = GHC.narrowWord8# x
#endif
{-# INLINE [0] fromWord8# #-}

-- Word8# - Conversion ---------------------------------------------------------

-- | Cast an 'Word#' value to an 'Word8#' value.
--
-- @since 1.0.0
wordToWord8# :: Word# -> Word8#
#if (MIN_VERSION_ghc_prim(0,8,0))
wordToWord8# = GHC.wordToWord8#
#else
wordToWord8# = GHC.narrowWord8# 
#endif

-- | Cast an 'Word8#' value to an 'Word#' value.
--
-- @since 1.0.0
word8ToWord# :: Word8# -> Word#
#if (MIN_VERSION_ghc_prim(0,8,0))
word8ToWord# = GHC.word8ToWord#
#else
word8ToWord# = GHC.extendWord8# 
#endif

-- Word16# ---------------------------------------------------------------------

-- | Convert a unboxed 'Word16#' value to a boxed 'Word16' value.
--
-- @since 1.0.0
fromWord16# :: Word16# -> Word16
#if (MIN_VERSION_ghc_prim(0,8,0))
fromWord16# = W16#
#else 
fromWord16# x = W16# (GHC.extendWord16# x)
#endif
{-# INLINE [0] fromWord16# #-}

-- | Convert a boxed 'Word16' value to an unboxed 'Word16#' value.
--
-- @since 1.0.0
toWord16# :: Word16 -> Word16#
#if (MIN_VERSION_ghc_prim(0,8,0))
toWord16# (W16# x) = x
#else 
toWord16# (W16# x) = GHC.narrowWord16# x
#endif
{-# INLINE [0] toWord16# #-}

-- Word16# - Conversion --------------------------------------------------------

-- | Cast an 'Word#' value to an 'Word16#' value.
--
-- @since 1.0.0
wordToWord16# :: Word# -> Word16#
#if (MIN_VERSION_ghc_prim(0,8,0))
wordToWord16# = GHC.wordToWord16#
#else
wordToWord16# = GHC.narrowWord16# 
#endif

-- | Cast an 'Word16#' value to an 'Word#' value.
--
-- @since 1.0.0
word16ToWord# :: Word16# -> Word#
#if (MIN_VERSION_ghc_prim(0,8,0))
word16ToWord# = GHC.word16ToWord#
#else
word16ToWord# = GHC.extendWord16# 
#endif

-- Word32# ---------------------------------------------------------------------

-- | Convert a unboxed 'Word32#' value to a boxed 'Word32' value.
--
-- @since 1.0.0
fromWord32# :: Word32# -> Word32
#if (MIN_VERSION_ghc_prim(0,8,0))
fromWord32# = W32#
#else 
fromWord32# x = W32# (word32ToWord# x)
#endif
{-# INLINE [0] fromWord32# #-}

-- | Convert a boxed 'Word32' value to an unboxed 'Word32#' value.
--
-- @since 1.0.0
toWord32# :: Word32 -> Word32#
#if (MIN_VERSION_ghc_prim(0,8,0))
toWord32# (W32# x) = x
#else 
toWord32# (W32# x) = wordToWord32# x 
#endif
{-# INLINE [0] toWord32# #-}

-- Word32# - Conversion --------------------------------------------------------

-- Note [Word32 Narrowings]
--
-- No conversion function between 'Word#' and 'Word32#' in versions of ghc-prim
-- earlier than 0.8.0. In order to implement versions of 'fromWord32#' and
-- 'toWord32#' that are compatible with earlier versions of ghc-prim,
-- 'unsafeCoerce#' must be used. The conversion functions 'wordToWord32#' and
-- 'word32ToWord#' could naively be defined as:
--
-- @
-- wordToWord32# :: Word# -> Word32#
-- wordToWord32# x = GHC.unsafeCoerce# x
--
-- word32ToWord# :: Word32# -> Word#
-- word32ToWord# x = GHC.unsafeCoerce# x
-- @
--
-- However, doing so will result would lead to undefined behavior and
-- inconsistent 'Word#' overflow. Placing 'narrow32Word#' around 'unsafeCoerce#'
-- is the only way to ensure the value of the word is perserved through the
-- cast.

-- | Cast an 'Word#' value to an 'Word32#' value.
--
-- @since 1.0.0
wordToWord32# :: Word# -> Word32#
#if (MIN_VERSION_ghc_prim(0,8,0))
wordToWord32# = GHC.wordToWord32#
#else
word32ToWord# x = GHC.narrow32Word# (GHC.unsafeCoerce# x) -- see [Word32 Narrowings]
#endif

-- | Cast an 'Word32#' value to an 'Word#' value.
--
-- @since 1.0.0
word32ToWord# :: Word32# -> Word#
#if (MIN_VERSION_ghc_prim(0,8,0))
word32ToWord# = GHC.word32ToWord#
#else
wordToWord32# x = GHC.unsafeCoerce# (GHC.narrow32Word# x) -- see [Word32 Narrowings]
#endif

-- Word32# - Arithmetic --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
plusWord32# :: Word32# -> Word32# -> Word32#
#if (MIN_VERSION_ghc_prim(0,8,0))
plusWord32# = GHC.plusWord32# 
#else
plusWord32# a b = wordToWord32# (GHC.plusWord# (word32ToWord# a) (word32ToWord# b))
#endif

-- | TODO
--
-- @since 1.0.0
subWord32# :: Word32# -> Word32# -> Word32#
#if (MIN_VERSION_ghc_prim(0,8,0))
subWord32# = GHC.subWord32# 
#else
subWord32# a b = wordToWord32# (GHC.minusWord# (word32ToWord# a) (word32ToWord# b))
#endif

-- | TODO
--
-- @since 1.0.0
timesWord32# :: Word32# -> Word32# -> Word32#
#if (MIN_VERSION_ghc_prim(0,8,0))
timesWord32# = GHC.timesWord32# 
#else
timesWord32# a b = wordToWord32# (GHC.timesWord# (word32ToWord# a) (word32ToWord# b))
#endif

-- Word32# - Comparison --------------------------------------------------------

#if !(MIN_VERSION_ghc_prim(0,8,0))

gtWord32# :: Word32# -> Word32# -> Int#
gtWord32# a b = GHC.gtWord# (word32ToWord# a) (word32ToWord# b)

geWord32# :: Word32# -> Word32# -> Int#
geWord32# a b = GHC.geWord# (word32ToWord# a) (word32ToWord# b)

eqWord32# :: Word32# -> Word32# -> Int#
eqWord32# a b = GHC.eqWord# (word32ToWord# a) (word32ToWord# b)

neWord32# :: Word32# -> Word32# -> Int#
neWord32# a b = GHC.neWord# (word32ToWord# a) (word32ToWord# b)

ltWord32# :: Word32# -> Word32# -> Int#
ltWord32# a b = GHC.ltWord# (word32ToWord# a) (word32ToWord# b)

leWord32# :: Word32# -> Word32# -> Int#
leWord32# a b = GHC.leWord# (word32ToWord# a) (word32ToWord# b)

#endif