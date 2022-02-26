module Main where

import Expr
import Converter


main :: IO ()
main = do 
    putStrLn "Write your expression:"
    s <- getLine 
    if s == ":q" 
        then return() 
        else do 
            let exp = read s :: Expr
            putStr "NNF: " 
            putStrLn $ show $ toNNF exp
            putStr "DNF: " 
            putStrLn $ show $ toDNF exp 
            putStr "CNF: " 
            putStrLn $ show $ toCNF exp 
            main