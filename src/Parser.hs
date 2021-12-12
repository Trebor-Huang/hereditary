module Parser where
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isAlphaNum)
import SetFuck

-- A parser for a term
-- # starts a line of comments
{-
Term ::= IDENT
       | IDENT "(" ((Term ",")* Term)? ")"
       | Term "==" Term
       | Term "in" Term  -- These two infixes need special casing
       | "if" Term "then" Term "else" Term "end"
       | "{" IDENT ":" TERM "|" TERM "}"  -- Spec
       | "{" TERM "|" IDENT ":" TERM "}"  -- Repl
IDENT ::= [a-zA-Z_][a-zA-Z0-9_]*
-}

skipComment :: ReadP ()
skipComment = do
    skipSpaces
    do
        char '#'
        manyTill (satisfy (const True)) (char '\n')
        return ()
    <++ return ()

ident :: ReadP String
ident = do
    c <- satisfy (\c -> isAlpha c || c == '_')
    cs <- munch (\c -> isAlphaNum c || c == '_')
    if (c:cs) == "in" || (c:cs) == "if" || (c:cs) == "then" || (c:cs) == "else" || (c:cs) == "end"
        then pfail
    else return (c:cs)

-- "in" binds tighter than "=="
noEq :: ReadP Term
noEq = do
    ts <- sepBy1 noinfix (skipComment >> string "in")
    return (foldl1 Elem ts)

term :: ReadP Term
term = do
    ts <- sepBy1 noEq (skipComment >> string "==")
    return (foldl1 Eq ts)

noinfix :: ReadP Term
noinfix = do
        skipComment
        char '('
        t <- term
        skipComment
        char ')'
        return t
    <++ do
        skipComment
        c <- ident
        skipComment
        char '('
        ts <- sepBy term (skipComment >> char ',')
        skipComment
        char ')'
        return (Const c ts)
    <++ (skipComment >> Var <$> ident)
    <++ do
        skipComment
        string "if"
        t1 <- term
        skipComment
        string "then"
        t2 <- term
        skipComment
        string "else"
        t3 <- term
        string "end"
        return (If t1 t2 t3)
    <++ do
        skipComment
        string "{"
        skipComment
        c <- ident
        skipComment
        string ":"
        t1 <- term
        skipComment
        string "|"
        t2 <- term
        skipComment
        string "}"
        return (Spec c t2 t1)
    <++ do
        skipComment
        string "{"
        t1 <- term
        skipComment
        string "|"
        skipComment
        c <- ident
        skipComment
        string ":"
        t2 <- term
        skipComment
        string "}"
        return (Repl c t1 t2)

{-
Program ::= (Declare ";")*
Declare ::= IDENT "(" ((IDENT ",")* IDENT)? ")" "=" Term
-}

declare :: ReadP (ConstName, ([ConstName], Term))
declare = do
    skipComment
    c <- ident
    skipComment
    char '('
    cs <- sepBy (skipComment >> ident) (skipComment >> char ',')
    skipComment
    char ')'
    skipComment
    char '='
    t <- term
    return (c, (cs, t))

program :: ReadP Program
program = do 
    p <- endBy declare (skipComment >> char ';')
    skipComment
    eof
    return p
