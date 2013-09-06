hen [![Build Status][travis-img]][travis]
===

[travis]: http://travis-ci.org/selectel/hen
[travis-img]: https://secure.travis-ci.org/selectel/hen.png

`hen` is a Haskell bindings to Xen hypervisor interface. Key feature of
`hen` is that you can use a single library to interact with both Xen3 and
Xen4 hypervisor versions.

The API is split into three levels:

```
|-----------------+-------------------------------------------------------------|
| Level           | Description                                                 |
|-----------------+-------------------------------------------------------------|
| System.Xen.Low  | Provides bindings to raw xenctrl calls.                     |
| System.Xen.Mid  | Defines useful helper functions for acessing System.Xen.Low |
|                 | and allows you to use your favourite impure Monad.          |
| System.Xen.High | Implements the Xen monad, which guarantees safety during    |
|                 | lowlevel operations.                                        |
|-----------------+-------------------------------------------------------------|
```

Example
-------

```haskell
module Main (main) where

import System.Xen (runXenT, domainGetInfo)

main :: IO ()
main = print =<< runXenT domainGetInfo
```

Installation
------------

`hen` requires `xenctrl` headers, Google and unpack any of the two packages bellow:

```
+-----------+-----------------------------------------+
|XCP version|                   Package               |
|-----------+-----------------------------------------+
|1.1        |xen-devel-3.4.2-1.1.0.704.20055.i686.rpm |
|1.5        |xen-devel-4.1.3-1.6.10.498.23551.i686.rpm|
+-----------+-----------------------------------------+
```

After you've unpacked the headers, point `cabal-dev` to them and you're done:

```bash
λ cabal-dev configure --extra-include-dirs=/path/to/headers
```
