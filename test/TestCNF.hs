module TestCNF
    ( 
        testsCNF
    ) where
import Test.HUnit
import System.Exit
import TestCommon
import Expr
import Converter
import CheckConverter

test_checkCNF_True = TestCase (assertBool (problem_in_fun_ea "checkCNF" exp True res) res)
    where exp = read "(~y \\/ ~z \\/ w) /\\ (x \\/ ~z)"::Expr
          res = checkCNF exp

test_checkCNF_False1 = TestCase (assertBool (problem_in_fun_ea "checkCNF" exp False res) (not res)) 
    where exp = read "((a /\\ b) \\/ z) /\\ (x /\\ z)"::Expr
          res = checkCNF exp

test_checkCNF_False2 = TestCase (assertBool (problem_in_fun_ea "checkCNF" exp False res) (not res))
    where exp = read "(~(y \\/ z)) /\\ (x \\/ z)"::Expr
          res = checkCNF exp

test_toCNF_isCNF1 = TestCase (assertBool (problem_in_fun_ea "toCNF (isCNF)" exp_original True res) (res))
    where exp_original = read "~((y -> z) <-> w)"::Expr
          exp_cnf = toCNF exp_original
          res = checkCNF exp_cnf

test_toCNF_isCNF2 = TestCase (assertBool (problem_in_fun_ea "toCNF (isCNF)" exp_original True res) (res))
    where exp_original = read "~(y -> z) \\/ (a <-> b)) /\\ ~(~c -> b)"::Expr
          exp_cnf = toCNF exp_original
          res = checkCNF exp_cnf

test_toCNF_eval1 = TestCase (assertBool (problem_in_fun "toCNF (eval)" exp_original) (res))
    where exp_original = read "~((y -> z) <-> w)"::Expr
          exp_cnf = toCNF exp_original
          res = checkEqEval exp_original exp_cnf

test_toCNF_eval2 = TestCase (assertBool (problem_in_fun "toCNF (eval)" exp_original) (res))
    where exp_original = read "~(y -> z) \\/ (a <-> b)) /\\ ~(~c -> b)"::Expr
          exp_cnf = toCNF exp_original
          res = checkEqEval exp_original exp_cnf

testsCNF = TestList [
    TestLabel "test fun: checkCNF" test_checkCNF_True, 
    TestLabel "test fun: checkCNF" test_checkCNF_False1,
    TestLabel "test fun: checkCNF" test_checkCNF_False2, 
    TestLabel "test fun: toCNF (is CNF form)" test_toCNF_isCNF1, 
    TestLabel "test fun: toCNF (is CNF form)" test_toCNF_isCNF2,
    TestLabel "test fun: toCNF (eval)" test_toCNF_eval1,
    TestLabel "test fun: toCNF (eval)" test_toCNF_eval2 ]