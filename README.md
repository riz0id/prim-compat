<div align="center">

# prim-compat: lightweight ghc-prim compatibility wrapper. 

[![GHC 9.2.2](https://github.com/riz0id/prim-compat/actions/workflows/ghc922.yml/badge.svg)](https://github.com/riz0id/prim-compat/actions/workflows/ghc922.yml) 
[![GHC 9.0.2](https://github.com/riz0id/prim-compat/actions/workflows/ghc902.yml/badge.svg)](https://github.com/riz0id/prim-compat/actions/workflows/ghc902.yml)
[![GHC 8.10.7](https://github.com/riz0id/prim-compat/actions/workflows/ghc8107.yml/badge.svg)](https://github.com/riz0id/prim-compat/actions/workflows/ghc8107.yml)

</div>

## Overview

This library is a minimal wrapper over [`ghc-prim`](https://hackage.haskell.org/package/ghc-prim) that manages the compatibility of prim-ops across a wide range of GHC versions. `prim-compat` can be used as a drop-in replacement of `ghc-prim` for packages that need to depend on prim-ops directly, without requiring the package to safely manage missing prim-ops across different GHC versions or restricting `base` package bounds.

