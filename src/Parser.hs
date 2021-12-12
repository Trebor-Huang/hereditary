module Parser where
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isAlphaNum)
import SetFuck

-- A parser for a term
{-
Term ::= IDENT
       | IDENT "(" ((Term ",")* Term)? ")"
       | Term "==" Term
       | Term "in" Term  -- These two infixes need special casing
       | "if" Term "then" Term "else" Term
       | "{" IDENT ":" TERM "|" TERM "}"  -- Spec
       | "{" TERM "|" IDENT ":" TERM "}"  -- Repl
IDENT ::= [a-zA-Z_][a-zA-Z0-9_]*
-}

ident :: ReadP String
ident = do
    c <- satisfy (\c -> isAlpha c || c == '_')
    cs <- munch (\c -> isAlphaNum c || c == '_')
    if (c:cs) == "in" || (c:cs) == "if" || (c:cs) == "then" || (c:cs) == "else"
        then pfail
    else return (c:cs)

-- "in" binds tighter than "=="
noEq :: ReadP Term
noEq = do
    ts <- sepBy1 noinfix (skipSpaces >> string "in")
    return (foldl1 Elem ts)

term :: ReadP Term
term = do
    ts <- sepBy1 noEq (skipSpaces >> string "==")
    return (foldl1 Eq ts)

noinfix :: ReadP Term
noinfix = do
        skipSpaces
        char '('
        t <- term
        skipSpaces
        char ')'
        return t
    <++ do
        skipSpaces
        c <- ident
        skipSpaces
        char '('
        ts <- sepBy term (skipSpaces >> char ',')
        skipSpaces
        char ')'
        return (Const c ts)
    <++ (skipSpaces >> Var <$> ident)
    <++ do
        skipSpaces
        string "if"
        t1 <- term
        skipSpaces
        string "then"
        t2 <- term
        skipSpaces
        string "else"
        t3 <- term
        return (If t1 t2 t3)
    <++ do
        skipSpaces
        string "{"
        skipSpaces
        c <- ident
        skipSpaces
        string ":"
        t1 <- term
        skipSpaces
        string "|"
        t2 <- term
        skipSpaces
        string "}"
        return (Spec c t2 t1)
    <++ do
        skipSpaces
        string "{"
        t1 <- term
        skipSpaces
        string "|"
        skipSpaces
        c <- ident
        skipSpaces
        string ":"
        t2 <- term
        skipSpaces
        string "}"
        return (Repl c t1 t2)

{-
Program ::= (Declare ";")*
Declare ::= IDENT "(" ((IDENT ",")* IDENT)? ")" "=" Term
-}

declare :: ReadP (ConstName, ([ConstName], Term))
declare = do
    skipSpaces
    c <- ident
    skipSpaces
    char '('
    cs <- sepBy (skipSpaces >> ident) (skipSpaces >> char ',')
    skipSpaces
    char ')'
    skipSpaces
    char '='
    t <- term
    return (c, (cs, t))

program :: ReadP Program
program = do 
    p <- many (do
        d <- declare
        skipSpaces
        char ';'
        return d)
    skipSpaces
    eof
    return p
