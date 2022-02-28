module RandomExpr
    ( 
        tests_rnd
    ) where
import Test.HUnit
import System.Exit
import TestCommon
import Expr
import Converter
import CheckConverter
import System.Random


rnd_vars = ["e", "w", "x", "y", "z"]
nexpr = 50
maxh = 4
maxsize = 2^(maxh + 1)

rnd_expr :: [Expr]
rnd_expr = make_expr lns nexpr
    where lns = randoms (mkStdGen 239) :: [Int] 

make_expr :: [Int] -> Int -> [Expr]
make_expr lns n | n == 0 = [] 
                | otherwise = (make_one_expr rnd_lst h) : (make_expr new_lns (n - 1))
        where h = (lns !! 0) `mod` maxh + 1
              gen = lns !! 1
              new_lns = drop 2 lns
              rnd_lst = fmap (\x -> x `mod` 5) (randoms (mkStdGen gen) :: [Int])

make_one_expr rnd_lst h | h == 0 = (Atom (Var (rnd_vars !! pos)))
                        | otherwise = case op of 
                            0 -> Not (make_one_expr rnd_lst1 (h - 1))
                            1 -> (make_one_expr rnd_lst1 (h - 1)) :& (make_one_expr rnd_lst2 (h - 1))
                            2 -> (make_one_expr rnd_lst1 (h - 1)) :| (make_one_expr rnd_lst2 (h - 1))
                            3 -> (make_one_expr rnd_lst1 (h - 1)) :=> (make_one_expr rnd_lst2 (h - 1))
                            _ -> (make_one_expr rnd_lst1 (h - 1)) :<=> (make_one_expr rnd_lst2 (h - 1))
        where pos = (head rnd_lst)
              op = (head rnd_lst) 
              rnd_lst1 = drop 1 rnd_lst
              rnd_lst2 = drop maxsize rnd_lst1


test_rnd_fn toF checkF = all (== True) $ do 
    exp <- rnd_expr 
    let f_exp = toF exp 
    return $ (checkF f_exp) && (checkEqEval exp f_exp)

test_rnd_NNF = TestCase (assertBool "problem with NNF" (test_rnd_fn toNNF checkNNF))
test_rnd_DNF = TestCase (assertBool "problem with DNF" (test_rnd_fn toDNF checkDNF)) 
test_rnd_CNF = TestCase (assertBool "problem with CNF" (test_rnd_fn toCNF checkCNF)) 

tests_rnd = TestList [
    TestLabel "test NNF on rnd expr" test_rnd_NNF,
    TestLabel "test DNF on rnd expr" test_rnd_DNF,
    TestLabel "test CNF on rnd expr" test_rnd_CNF ]