{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_gauss_parallel (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "gauss_parallel"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Parallel Gaussian elimination in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
