module TestCommon
    ( 
        problem_in_fun_ea,
        problem_in_fun,
        checkEqEval, 
        (+|+),
        impls, vars
    ) where
import Expr
import Test.HUnit
import System.Exit
import System.Random

problem_in_fun_ea :: Show a => String -> Expr -> a -> a -> String 
problem_in_fun_ea s exp expected actual = (problem_in_fun s exp) ++ 
    "\nExpected: " ++ show expected ++ "   Actual: " ++ show actual

problem_in_fun :: String -> Expr -> String
problem_in_fun s exp = s ++ " problem with expression " ++ show exp

checkEqEval :: Expr -> Expr -> Bool 
checkEqEval expE expA = all (== True) $ do 
    impl <- impls
    return $ (eval impl expE) == (eval impl expA)

-- only so sum TestList with TestList
infixl 5 +|+
(+|+) :: Test -> Test -> Test
(TestList a) +|+ (TestList b) = TestList $ a ++ b

-- Not very good solution, but ok for first testing
impls :: [Inter]
impls = make_impls rnd_lst nimpl

make_impls :: [Bool] -> Int -> [Inter]
make_impls lst n | n == 0 = [] 
                 | otherwise = fcur : (make_impls new_lst (n - 1))
        where vars_sz = length vars 
              new_lst = drop vars_sz lst 
              cur_lst = take vars_sz lst
              fcur = \s -> let res = lookup s (zip vars [0..]) in case res of
                  (Just j) -> cur_lst !! j 
                  _ -> False

vars = map (\c -> [c]) ['a'..'z']
nimpl = 1000
rnd_lst = randoms (mkStdGen 100) :: [Bool]