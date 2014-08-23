{-|
Module       : Main
Description  : Run tests for the library
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

main = return ()

-- main = defaultMain tests

-- tests =
--   [ testGroup "Agent" [ testProperty "" prop_prop0
--                       , testProperty "propname1" prop_prop1
--                       ]
--   ]

-- -- prop_sort1 xs = sort xs = sortBy compare xs
