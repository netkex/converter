module TestDNF
    ( 
        testsDNF
    ) where
import Test.HUnit
import System.Exit
import TestCommon
import Expr
import Converter
import CheckConverter

test_checkDNF_True = TestCase (assertBool (problem_in_fun_ea "checkDNF" exp True res) res)
    where exp = read "(~y /\\ ~z /\\ w) \\/ (x /\\ ~z)"::Expr
          res = checkDNF exp

test_checkDNF_False1 = TestCase (assertBool (problem_in_fun_ea "checkDNF" exp False res) (not res)) 
    where exp = read "((a \\/ b) /\\ z) \\/ (x \\/ z)"::Expr
          res = checkDNF exp

test_checkDNF_False2 = TestCase (assertBool (problem_in_fun_ea "checkDNF" exp False res) (not res))
    where exp = read "(~(y /\\ z)) \\/ (x /\\ z)"::Expr
          res = checkDNF exp

test_toDNF_isDNF1 = TestCase (assertBool (problem_in_fun_ea "toDNF (isDNF)" exp_original True res) (res))
    where exp_original = read "~((y -> z) <-> w)"::Expr
          exp_dnf = toDNF exp_original
          res = checkDNF exp_dnf

test_toDNF_isDNF2 = TestCase (assertBool (problem_in_fun_ea "toDNF (isDNF)" exp_original True res) (res))
    where exp_original = read "~(y -> z) \\/ (a <-> b)) /\\ ~(~c -> b)"::Expr
          exp_dnf = toDNF exp_original
          res = checkDNF exp_dnf

test_toDNF_eval1 = TestCase (assertBool (problem_in_fun "toDNF (eval)" exp_original) (res))
    where exp_original = read "~((y -> z) <-> w)"::Expr
          exp_dnf = toDNF exp_original
          res = checkEqEval exp_original exp_dnf

test_toDNF_eval2 = TestCase (assertBool (problem_in_fun "toDNF (eval)" exp_original) (res))
    where exp_original = read "~(y -> z) \\/ (a <-> b)) /\\ ~(~c -> b)"::Expr
          exp_dnf = toDNF exp_original
          res = checkEqEval exp_original exp_dnf

testsDNF = TestList [
    TestLabel "test fun: checkDNF" test_checkDNF_True, 
    TestLabel "test fun: checkDNF" test_checkDNF_False1,
    TestLabel "test fun: checkDNF" test_checkDNF_False2, 
    TestLabel "test fun: toDNF (is DNF form)" test_toDNF_isDNF1, 
    TestLabel "test fun: toDNF (is DNF form)" test_toDNF_isDNF2,
    TestLabel "test fun: toDNF (eval)" test_toDNF_eval1,
    TestLabel "test fun: toDNF (eval)" test_toDNF_eval2 ]