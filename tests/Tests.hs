module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified System.Xen.Types.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ System.Xen.Types.Tests.tests
    ]
