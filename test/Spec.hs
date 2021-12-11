import SetFuck
import Hereditary

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

main :: IO ()
main = do
    -- input <- readLn
    print $ (fromSet (run test []) :: (Int, Int))
