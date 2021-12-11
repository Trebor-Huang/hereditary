import SetFuck
import Hereditary
import Parser
import Text.ParserCombinators.ReadP (readP_to_S)

test :: Program
test = [
    ("zero", ([], Empty)),
    ("one", ([], Power Empty)),
    ("two", ([], Power (Power Empty))),
    ("pair", (["x", "y"], Repl "z" (If (Var "z") (Var "x") (Var "y"))
        (Const "two" []))),
    ("opair", (["x", "y"], Const "pair"
        [ Const "pair" [Var "x", Var "x"],
          Const "pair" [Var "x", Var "y"]])),
    ("succ", (["x"], Union (Const "pair" [Var "x", Const "pair" [Var "x", Var "x"]]))),
    ("main", ([], Const "opair"
        [Const "two" [],
         Const "succ" [Const "two" []]]))
    ]

source = "superb(x, y ) = succ( x== y in x,opair(x,\n   y));"
-- Extra whitespace is to make sure the parser doesn't get confused by
-- the newline.

main :: IO ()
main = do
    -- input <- readLn
    -- print $ (fromSet (run test []) :: (Int, Int))
    print(readP_to_S program source)
