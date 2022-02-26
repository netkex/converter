module CheckConverter
    ( 
        checkNNF,
        checkDNF,
        checkCNF
    ) where
import Expr

data Level = Outer | Inner

checkNNF (Atom x) = True
checkNNF (Not (Atom x)) = True 
checkNNF (expl :& expr) = checkNNF expl && checkNNF expr 
checkNNF (expl :| expr) = checkNNF expl && checkNNF expr 
checkNNF _              = False

checkDNF exp = checkDNF_ Outer exp

checkDNF_ Outer (expl :| expr) = checkDNF_ Outer expl && checkDNF_ Outer expr 
checkDNF_ Outer exp@(expl :& expr) = checkDNF_ Inner exp
checkDNF_ Inner (expl :& expr) = checkDNF_ Inner expl && checkDNF_ Inner expr
checkDNF_ _ (Atom x) = True
checkDNF_ _ (Not (Atom x)) = True 
checkDNF_ _ _ = False 

checkCNF exp = checkCNF_ Outer exp 

checkCNF_ Outer (expl :& expr) = checkCNF_ Outer expl && checkCNF_ Outer expr 
checkCNF_ Outer exp@(expl :| expr) = checkCNF_ Inner exp 
checkCNF_ Inner (expl :| expr) = checkCNF_ Inner expl && checkCNF_ Inner expr 
checkCNF_ _ (Atom x) = True 
checkCNF_ _ (Not (Atom x)) = True 
checkCNF_ _ _ = False
