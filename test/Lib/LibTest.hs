module Lib.LibTest (libTests) where

import Lib.SetCategoryTest
import Test.Tasty

libTests :: TestTree
libTests =
  testGroup
    "Lib"
    [Lib.SetCategoryTest.tests]
