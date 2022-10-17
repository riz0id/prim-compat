{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Int.Prim.Compat
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- "Data.Int.Prim.Compat" is a backwards compatibility layer over "GHC.Prim"
-- re-exporting types and functions related to the unboxed integer types 'Int#',
-- 'Int8#', 'Int16#', and 'Int32#'.
--
-- @since 1.0.0
module Data.Int.Prim.Compat (
  -- * Int# #int#
  Int#,
  fromInt#,
  toInt#,

  -- * Int8# #int8#
  Int8#,
  fromInt8#,
  toInt8#,

  -- ** Conversion #int8-conversion#
  intToInt8#,
  int8ToInt#,

  -- * Int16# #int16#
  Int16#,
  fromInt16#,
  toInt16#,

  -- ** Conversion #int16-conversion#
  intToInt16#,
  int16ToInt#,

  -- * Int32# #int32#
  Int32#,
  fromInt32#,
  toInt32#,

  -- ** Conversion #int32-conversion#
  intToInt32#,
  int32ToInt#,

  -- ** Arithmetic #word32-arithmetic#
  plusInt32#,
  subInt32#,
  timesInt32#,

  -- ** Comparison #int32-comparison#
  gtInt32#,
  geInt32#,
  eqInt32#,
  neInt32#,
  ltInt32#,
  leInt32#,
) where

import GHC.Exts (Int#, Int16#, Int32#, Int8#)
import qualified GHC.Exts as GHC
import GHC.Int (Int (I#), Int16 (I16#), Int32 (I32#), Int8 (I8#))

#if (MIN_VERSION_ghc_prim(0,8,0))

import GHC.Exts (eqInt32#, geInt32#, gtInt32#, leInt32#, ltInt32#, neInt32#)

#endif

{-# RULES
"Int# -> Int -> Int#" forall x.
  toInt# (fromInt# x) =
    x
"Int8# -> Int8 -> Int8#" forall x.
  toInt8# (fromInt8# x) =
    x
"Int16# -> Int16 -> Int16#" forall x.
  toInt16# (fromInt16# x) =
    x
"Int32# -> Int32 -> Int32#" forall x.
  toInt32# (fromInt32# x) =
    x
  #-}

-- Int# ------------------------------------------------------------------------

-- | Convert a unboxed 'Int#' value to a boxed 'Int' value.
--
-- @since 1.0.0
fromInt# :: Int# -> Int
fromInt# = I#
{-# INLINE [0] fromInt# #-}

-- | Convert a boxed 'Int' value to an unboxed 'Int#' value.
--
-- @since 1.0.0
toInt# :: Int -> Int#
toInt# (I# x) = x
{-# INLINE [0] toInt# #-}

-- Int8# -----------------------------------------------------------------------

-- | Convert a unboxed 'Int8#' value to a boxed 'Int8' value.
--
-- @since 1.0.0
fromInt8# :: Int8# -> Int8
#if (MIN_VERSION_ghc_prim(0,8,0))
fromInt8# = I8#
#else 
fromInt8# x = I8# (GHC.extendInt8# x)
#endif
{-# INLINE [0] fromInt8# #-}

-- | Convert a boxed 'Int8' value to an unboxed 'Int8#' value.
--
-- @since 1.0.0
toInt8# :: Int8 -> Int8#
#if (MIN_VERSION_ghc_prim(0,8,0))
toInt8# (I8# x) = x
#else 
toInt8# (I8# x) = GHC.narrowInt8# x
#endif
{-# INLINE [0] toInt8# #-}

-- Int8# - Conversion ----------------------------------------------------------

-- | Cast an 'Int#' value to an 'Int8#' value.
--
-- @since 1.0.0
intToInt8# :: Int# -> Int8#
#if (MIN_VERSION_ghc_prim(0,8,0))
intToInt8# = GHC.intToInt8#
#else
intToInt8# = GHC.narrowInt8# 
#endif

-- | Cast an 'Int8#' value to an 'Int#' value.
--
-- @since 1.0.0
int8ToInt# :: Int8# -> Int#
#if (MIN_VERSION_ghc_prim(0,8,0))
int8ToInt# = GHC.int8ToInt#
#else
int8ToInt# = GHC.extendInt8# 
#endif

-- Int16# ----------------------------------------------------------------------

-- | Convert a unboxed 'Int16#' value to a boxed 'Int16' value.
--
-- @since 1.0.0
fromInt16# :: Int16# -> Int16
#if (MIN_VERSION_ghc_prim(0,8,0))
fromInt16# = I16#
#else 
fromInt16# x = I16# (GHC.extendInt16# x)
#endif
{-# INLINE [0] fromInt16# #-}

-- | Convert a boxed 'Int16' value to an unboxed 'Int16#' value.
--
-- @since 1.0.0
toInt16# :: Int16 -> Int16#
#if (MIN_VERSION_ghc_prim(0,8,0))
toInt16# (I16# x) = x
#else 
toInt16# (I16# x) = GHC.narrowInt16# x
#endif
{-# INLINE [0] toInt16# #-}

-- Int16# - Conversion ---------------------------------------------------------

-- | Cast an 'Int#' value to an 'Int16#' value.
--
-- @since 1.0.0
intToInt16# :: Int# -> Int16#
#if (MIN_VERSION_ghc_prim(0,8,0))
intToInt16# = GHC.intToInt16#
#else
intToInt16# = GHC.narrowInt16# 
#endif

-- | Cast an 'Int16#' value to an 'Int#' value.
--
-- @since 1.0.0
int16ToInt# :: Int16# -> Int#
#if (MIN_VERSION_ghc_prim(0,8,0))
int16ToInt# = GHC.int16ToInt#
#else
int16ToInt# = GHC.extendInt16# 
#endif

-- Int32# ----------------------------------------------------------------------

-- | Convert a unboxed 'Int32#' value to a boxed 'Int32' value.
--
-- @since 1.0.0
fromInt32# :: Int32# -> Int32
#if (MIN_VERSION_ghc_prim(0,8,0))
fromInt32# = I32#
#else 
fromInt32# x = I32# (int32ToInt# x)
#endif
{-# INLINE [0] fromInt32# #-}

-- | Convert a boxed 'Int32' value to an unboxed 'Int32#' value.
--
-- @since 1.0.0
toInt32# :: Int32 -> Int32#
#if (MIN_VERSION_ghc_prim(0,8,0))
toInt32# (I32# x) = x
#else 
toInt32# (I32# x) = intToInt32# x
#endif
{-# INLINE [0] toInt32# #-}

-- Int32# - Conversion ---------------------------------------------------------

-- Note [Int32 Narrowings]
--
-- No conversion function between 'Int#' and 'Int32#' in versions of ghc-prim
-- earlier than 0.8.0. In order to implement versions of 'fromInt32#' and
-- 'toInt32#' that are compatible with earlier versions of ghc-prim,
-- 'unsafeCoerce#' must be used. The conversion functions 'intToInt32#' and
-- 'int32ToInt#' could naively be defined as:
--
-- @
-- IntToInt32# :: Int# -> Int32#
-- IntToInt32# x = GHC.unsafeCoerce# x
--
-- Int32ToInt# :: Int32# -> Int#
-- Int32ToInt# x = GHC.unsafeCoerce# x
-- @
--
-- However, doing so will result would lead to undefined behavior and
-- inconsistent 'Int#' overflow. Placing 'narrow32Int#' around 'unsafeCoerce#'
-- is the only way to ensure the value of the Int is perserved through the
-- cast.

-- | Cast an 'Int#' value to an 'Int32#' value.
--
-- @since 1.0.0
intToInt32# :: Int# -> Int32#
#if (MIN_VERSION_ghc_prim(0,8,0))
intToInt32# = GHC.intToInt32#
#else
intToInt32# x = GHC.unsafeCoerce# (GHC.narrow32Int# x) -- see [Int32 Narrowings]
#endif

-- | Cast an 'Int32#' value to an 'Int#' value.
--
-- @since 1.0.0
int32ToInt# :: Int32# -> Int#
#if (MIN_VERSION_ghc_prim(0,8,0))
int32ToInt# = GHC.int32ToInt#
#else
int32ToInt# x = GHC.narrow32Int# (GHC.unsafeCoerce# x) -- see [Int32 Narrowings]
#endif

-- Int32# - Arithmetic ---------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
plusInt32# :: Int32# -> Int32# -> Int32#
#if (MIN_VERSION_ghc_prim(0,8,0))
plusInt32# = GHC.plusInt32# 
#else
plusInt32# a b = intToInt32# (int32ToInt# a GHC.+# int32ToInt# b)
#endif

-- | TODO
--
-- @since 1.0.0
subInt32# :: Int32# -> Int32# -> Int32#
#if (MIN_VERSION_ghc_prim(0,8,0))
subInt32# = GHC.plusInt32# 
#else
subInt32# a b = intToInt32# (int32ToInt# a GHC.-# int32ToInt# b)
#endif

-- | TODO
--
-- @since 1.0.0
timesInt32# :: Int32# -> Int32# -> Int32#
#if (MIN_VERSION_ghc_prim(0,8,0))
timesInt32# = GHC.timesInt32# 
#else
timesInt32# a b = intToInt32# (int32ToInt# a GHC.*# int32ToInt# b)
#endif

-- Int32# - Comparison ---------------------------------------------------------

#if !(MIN_VERSION_ghc_prim(0,8,0))

gtInt32# :: Int32# -> Int32# -> Int#
gtInt32# a b = (GHC.>#) (int32ToInt# a) (int32ToInt# b)

geInt32# :: Int32# -> Int32# -> Int#
geInt32# a b = (GHC.>=#) (int32ToInt# a) (int32ToInt# b)

eqInt32# :: Int32# -> Int32# -> Int#
eqInt32# a b = (GHC.==#) (int32ToInt# a) (int32ToInt# b)

neInt32# :: Int32# -> Int32# -> Int#
neInt32# a b = (GHC./=#) (int32ToInt# a) (int32ToInt# b)

ltInt32# :: Int32# -> Int32# -> Int#
ltInt32# a b = (GHC.<#) (int32ToInt# a) (int32ToInt# b)

leInt32# :: Int32# -> Int32# -> Int#
leInt32# a b = (GHC.<=#) (int32ToInt# a) (int32ToInt# b)

#endif