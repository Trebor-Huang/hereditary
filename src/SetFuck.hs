module SetFuck where
import Hereditary
import Control.Monad.Reader
type VarName = String
type ConstName = String

data Term
    = Var VarName
    | Const ConstName [Term]
    | Empty
    | Power Term
    | Eq Term Term
    | Elem Term Term
    | If Term Term Term
    | Spec VarName Term Term
    | Repl VarName Term Term
    deriving (Show, Eq, Read)

update :: Eq a => (a, b) -> Reader [(a, b)] c -> Reader [(a, b)] c
update (a, b) = local nudge
    where
        nudge [] = [(a, b)]
        nudge ((a', b') : env) = if a' == a then
                (a', b) : env
            else
                (a', b') : nudge env

updates :: Eq a => [(a, b)] -> Reader [(a, b)] c -> Reader [(a, b)] c
updates = foldr ((.) . update) id

eval :: [(ConstName, ([VarName], Term))] -> Term -> Reader [(VarName, Set)] Set
eval env (Var v) = do
    x <- asks $ lookup v
    maybe (error "Unknown variable") return x
eval env (Const c ts) = do
    let t = lookup c env
    case t of
      Nothing -> error "Unknown constant"
      Just (vs, tm) -> do
          xs <- mapM (eval env) ts
          updates (zip vs xs) $ eval env tm
eval env Empty = return empty
eval env (Power x) = power <$> eval env x
eval env (Eq x y) = do
    x' <- eval env x
    y' <- eval env y
    return (if x' == y' then true else empty)
eval env (Elem x y) = do
    x' <- eval env x
    y' <- eval env y
    return (if x' ∈ y' then true else empty)
eval env (If b x y) = do
    b' <- eval env b
    if b' /= empty then
        eval env x
    else
        eval env y
eval env (Spec v pred x) = do
    x' <- eval env x
    specification (\z -> (/= empty) <$> update (v, z) (eval env pred)) x'
eval env (Repl v func x) = do
    x' <- eval env x
    replace (\z -> update (v, z) (eval env func)) x'

type Program = [(ConstName, ([VarName], Term))]

run :: Program -> [Set] -> Set
run p input = case lookup "main" p of
  Nothing -> error "\"main\" function not found."
  Just (vs, main) -> runReader (eval p main) (zip vs input)
