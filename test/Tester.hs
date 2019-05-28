module Tester (
    runAll,
    runSuite
) where

import TokenizerTests(astTester, astTests, nfaTests, nfaTester)

import qualified Data.Map as Map
import Data.Maybe

makeTest :: ((i, e) -> Maybe String) -> [(i, e)] -> String -> IO ()
makeTest runner tests name = let results = foldl (\rs -> \r -> rs ++ "  " ++ r ++ "\n") "" $ catMaybes $ map runner tests in
                         if results == "" then putStr $ name ++ ": pass\n"
                                          else putStr $ name ++ ": fail\n" ++ results

runSuite :: String -> IO ()
runSuite name = case (Map.lookup name suites) of
                    Just suite -> suite name
                    Nothing -> do putStr $ "Suite \"" ++ name ++ "\"  not found."

runAll :: String -> IO ()
runAll _ = Map.foldlWithKey (\_ -> \name -> \suite -> suite name) (return ()) suites

suites :: Map.Map String (String -> IO ())
suites = Map.fromList [("Tokenizer AST", makeTest astTester astTests),
                       ("Tokenizer NFA", makeTest nfaTester nfaTests)]