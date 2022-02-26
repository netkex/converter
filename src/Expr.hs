module Expr
    ( 
        Atom (Var, Bottom, Top),
        Expr(Atom, Not, (:&), (:|), (:=>), (:<=>)),
        Vr,
        Inter, 
        atoms, 
        eval
    ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

type Vr = String
data Atom = Var Vr | Bottom | Top
type Inter = Vr -> Bool

infixr 5 :&
infixr 4 :| 
infixr 3 :=>
infixr 2 :<=>

data Expr = Atom Atom
    | Expr :& Expr
    | Expr :| Expr
    | Expr :=> Expr
    | Expr :<=> Expr
    | Not Expr

instance Show Expr where
       showsPrec _ (Atom (Var s)) = showString s 
       showsPrec _ (Not (Atom (Var s))) = showString $ "~" ++ s 
       showsPrec _ (Atom (Bottom)) = showString "F" 
       showsPrec _ (Atom (Top)) = showString "T" 
       showsPrec _ (Not (Atom (Bottom))) = showString "T" 
       showsPrec _ (Not (Atom (Top))) = showString "F"
       showsPrec _ (Not exp) = showString "~(" . showsPrec 0 exp . showString ")"

       showsPrec d (expl :& expr) = customParen d and_prec $
                showsPrec (and_prec) expl .
                showString " /\\ " . 
                showsPrec (and_prec) expr 
            where and_prec = 5

       showsPrec d (expl :| expr) = customParen d or_prec $
                showsPrec (or_prec) expl .
                showString " \\/ " . 
                showsPrec (or_prec) expr 
            where or_prec = 4

       showsPrec d (expl :=> expr) = customParen d (-1) $
                showsPrec (imp_prec) expl .
                showString " -> " . 
                showsPrec (imp_prec) expr 
            where imp_prec = 3
        
       showsPrec d (expl :<=> expr) = customParen d (-1) $
                showsPrec (eq_prec) expl .
                showString " <-> " . 
                showsPrec (eq_prec) expr 
            where eq_prec = 2


customParen :: Int -> Int -> ShowS -> ShowS
customParen d prec = showParen (d /= prec && d /= 0)


lexer = makeTokenParser $ haskellDef { 
        reservedOpNames = ["/\\", "\\/", "->", "<->"] 
    } 
expr  = buildExpressionParser table term
term  = parens lexer expr <|> (Atom <$> toAtom <$> identifier lexer)

table = [
          [prefix "~" Not],
          [binary "/\\"   (:&)    AssocRight],
          [binary "\\/"   (:|)    AssocRight],
          [binary "->"    (:=>)   AssocRight],
          [binary "<->"   (:<=>)  AssocRight]
        ]
    where
        binary  name fun assoc = Infix (do { reservedOp lexer name; return fun }) assoc
        prefix  name fun       = Prefix (do { reservedOp lexer name; return fun })


instance Read Expr where
    readsPrec _ s | (Right res) <- parse expr "" s = [(res, "")]
                  | otherwise                      = []



toAtom :: String -> Atom
toAtom "T" = Top
toAtom "F" = Bottom
toAtom s   = Var s

atoms :: Expr -> [Vr]
atoms (Atom (Var p)) = [p]
atoms (Not exp) = atoms exp
atoms (expl :& expr) = atomsLft ++ filter (`notElem` atomsLft) (atoms expr)
    where atomsLft = atoms expl
atoms (expl :| expr) = atomsLft ++ filter (`notElem` atomsLft) (atoms expr)
    where atomsLft = atoms expl
atoms (expl :=> expr) = atomsLft ++ filter (`notElem` atomsLft) (atoms expr)
    where atomsLft = atoms expl
atoms (expl :<=> expr) = atomsLft ++ filter (`notElem` atomsLft) (atoms expr)
    where atomsLft = atoms expl
atoms _ = []

eval :: Inter -> Expr -> Bool 
eval _ (Atom (Bottom)) = False 
eval _ (Atom (Top)) = True 
eval i (Atom (Var s)) = i s
eval i (Not exp) = not $ eval i exp
eval i (expl :| expr) = (eval i expl) || (eval i expr) 
eval i (expl :& expr) = (eval i expl) && (eval i expr)
eval i (expl :=> expr) = not (eval i expl) || (eval i expr)
eval i (expl :<=> expr) = (eval i expl) == (eval i expr)