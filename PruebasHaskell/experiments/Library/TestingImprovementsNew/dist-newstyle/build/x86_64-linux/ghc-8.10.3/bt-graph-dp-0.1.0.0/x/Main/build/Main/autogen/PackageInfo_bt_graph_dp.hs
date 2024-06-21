{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_bt_graph_dp (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "bt_graph_dp"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = ""
copyright :: String
copyright = "Copyright (C) 2021 Juan Pablo Royo Sales"
homepage :: String
homepage = "https://github.com/jproyo/bt-graph-dp#readme"
