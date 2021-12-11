import SetFuck
import Hereditary

test :: Program
test = [
    ("zero", ([], Empty)),
    ("one", ([], Power Empty)),
    ("two", ([], Power (Power Empty))),
    ("pair", (["x", "y"], Repl "z" (If (Var "z") (Var "x") (Var "y"))
        (Const "two" []))),
    ("main", (["x", "y"], Const "pair" [Var "x", Var "y"]))
    ]

main :: IO ()
main = do
    -- input <- readLn
    print (run test [empty, empty])
