module Converter
    ( 
        notPush,
        toNNF,
        toDNF, 
        toCNF,
    ) where

import Expr

notPush :: Expr -> Expr 
notPush (Not (expl :& expr)) = (Not expl) :| (Not expr)
notPush (Not (expl :| expr)) = (Not expl) :& (Not expr) 
notPush (Not (expl :=> expr)) = (expl) :& (Not expr)
notPush (Not (expl :<=> expr)) = 
    notPush $ Not ((expl :=> expr) :& (expr :=> expl))
notPush (Not (Not exp)) = exp 
notPush exp = exp 

toNNF :: Expr -> Expr 
toNNF exp = let nexp = notPush exp in case nexp of 
    (expl :& expr) -> toNNF expl :& toNNF expr 
    (expl :| expr) -> toNNF expl :| toNNF expr 
    (expl :=> expr) -> toNNF (Not expl) :| toNNF expr 
    (expl :<=> expr) -> toNNF $ (expl :=> expr) :& (expr :=> expl)
    exp -> exp

toDNF :: Expr -> Expr 
toDNF exp = let nfexp = toNNF exp in case nfexp of 
    (expl :| expr) -> toDNF expl :| toDNF expr 
    (expl :& expr) -> let dfexpl = toDNF expl; dfexpr = toDNF expr in case dfexpl of 
        (expll :| explr) -> toDNF $ (expll :& dfexpr) :| (explr :& dfexpr) 
        _ -> case dfexpr of 
            (exprl :| exprr) -> toDNF $ (dfexpl :& exprl) :| (dfexpl :& exprr) 
            _ -> dfexpl :& dfexpr
    exp -> exp

toCNF :: Expr -> Expr 
toCNF exp = let nfexp = toNNF exp in case nfexp of 
    (expl :& expr) -> toCNF expl :& toCNF expr 
    (expl :| expr) -> let cfexpl = toCNF expl; cfexpr = toCNF expr in case cfexpl of 
        (expll :& explr) -> toCNF $ (expll :| cfexpr) :& (explr :| cfexpr) 
        _ -> case cfexpr of 
            (exprl :& exprr) -> toCNF $ (cfexpl :| exprl) :& (cfexpl :| exprr) 
            _ -> cfexpl :| cfexpr
    exp -> exp