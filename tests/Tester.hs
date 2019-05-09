module Tester (
    runTests
) where

import Tests

runTests :: String -> IO ()
runTests suite = (head Tests.suites) suite