module TestNNF
    ( 
        testsNNF
    ) where
import Test.HUnit
import System.Exit
import TestCommon
import Expr
import Converter
import CheckConverter


test_checkNNF_True = TestCase (assertBool (problem_in_fun_ea "checkNNF" exp True res) res)
    where exp = read "(y \\/ z) /\\ (x \\/ z)"::Expr
          res = checkNNF exp

test_checkNNF_False1 = TestCase (assertBool (problem_in_fun_ea "checkNNF" exp False res) (not res)) 
    where exp = read "(y -> z) /\\ (x \\/ z)"::Expr
          res = checkNNF exp

test_checkNNF_False2 = TestCase (assertBool (problem_in_fun_ea "checkNNF" exp False res) (not res))
    where exp = read "~(y /\\ z) /\\ (x \\/ z)"::Expr
          res = checkNNF exp

test_toNNF_isNNF1 = TestCase (assertBool (problem_in_fun_ea "toNNF (isNNF)" exp_original True res) (res))
    where exp_original = read "~(y -> z)"::Expr
          exp_nnf = toNNF exp_original
          res = checkNNF exp_nnf

test_toNNF_isNNF2 = TestCase (assertBool (problem_in_fun_ea "toNNF (isNNF)" exp_original True res) (res))
    where exp_original = read "~(y -> z) \\/ (a <-> b)) /\\ ~(~c -> b)"::Expr
          exp_nnf = toNNF exp_original
          res = checkNNF exp_nnf

test_toNNF_eval1 = TestCase (assertBool (problem_in_fun "toNNF (eval)" exp_original) (res))
    where exp_original = read "~(y -> z)"::Expr
          exp_nnf = toNNF exp_original
          res = checkEqEval exp_original exp_nnf

test_toNNF_eval2 = TestCase (assertBool (problem_in_fun "toNNF (eval)" exp_original) (res))
    where exp_original = read "~(y -> z) \\/ (a <-> b)) /\\ ~(~c -> b)"::Expr
          exp_nnf = toNNF exp_original
          res = checkEqEval exp_original exp_nnf

testsNNF = TestList [
    TestLabel "test fun: checkNNF" test_checkNNF_True, 
    TestLabel "test fun: checkNNF" test_checkNNF_False1,
    TestLabel "test fun: checkNNF" test_checkNNF_False2, 
    TestLabel "test fun: toNNF (is NNF form)" test_toNNF_isNNF1, 
    TestLabel "test fun: toNNF (is NNF form)" test_toNNF_isNNF2,
    TestLabel "test fun: toNNF (eval)" test_toNNF_eval1,
    TestLabel "test fun: toNNF (eval)" test_toNNF_eval2 ]