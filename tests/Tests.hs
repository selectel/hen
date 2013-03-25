module Main where

import Test.Framework (defaultMain)

import qualified System.Xen.Types.Tests

main :: IO ()
main = defaultMain
    [ System.Xen.Types.Tests.tests
    ]
