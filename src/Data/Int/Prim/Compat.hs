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
module Data.Int.Prim.Compat 
  ( -- * Int#
    Int#,
    fromInt#,
    toInt#,

    -- * Int8#
    Int8#,
    fromInt8#,
    toInt8#,

    -- * Int16#
    Int16#,
    fromInt16#,
    toInt16#,

    -- * Int32#
    Int32#,
    fromInt32#,
    toInt32#,
  )
where

import GHC.Int (Int (I#), Int16 (I16#), Int32 (I32#), Int8 (I8#))
import GHC.Exts (Int#, Int16#, Int32#, Int8#)

#if !(MIN_VERSION_ghc_prim(0,8,0))

import qualified GHC.Exts as GHC

#endif

-- Int# ------------------------------------------------------------------------

fromInt# :: Int# -> Int
fromInt# = I# 

toInt# :: Int -> Int#
toInt# (I# x) = x

-- Int8# -----------------------------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

fromInt8# :: Int8# -> Int8
fromInt8# = I8#

toInt8# :: Int8 -> Int8#
toInt8# (I8# x) = x

#else 

fromInt8# :: Int8# -> Int8
fromInt8# x = I8# (GHC.extendInt8# x)

toInt8# :: Int8 -> Int8#
toInt8# (I8# x) = GHC.narrowInt8# x

#endif

-- Int16# ----------------------------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

fromInt16# :: Int16# -> Int16
fromInt16# = I16#

toInt16# :: Int16 -> Int16#
toInt16# (I16# x) = x

#else 

fromInt16# :: Int16# -> Int16
fromInt16# x = I16# (GHC.extendInt16# x)

toInt16# :: Int16 -> Int16#
toInt16# (I16# x) = GHC.narrowInt16# x

#endif

-- Int32# ----------------------------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

fromInt32# :: Int32# -> Int32
fromInt32# = I32#

toInt32# :: Int32 -> Int32#
toInt32# (I32# x) = x

#else 

fromInt32# :: Int32# -> Int32
fromInt32# x = I32# (GHC.unsafeCoerce# x)

toInt32# :: Int32 -> Int32#
toInt32# (I32# x) = GHC.unsafeCoerce# x

#endif