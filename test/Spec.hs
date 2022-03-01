import Test.HUnit
import System.Exit

import CheckConverter
import Converter
import Expr
import TestCommon
import TestNNF
import TestDNF
import TestCNF
import RandomExpr

main :: IO ()
main = do
    counts <- runTestTT tests
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure

tests = testsNNF +|+ testsDNF +|+ testsCNF +|+ tests_rnd