module Main where

import SetFuck
import Parser
import Text.ParserCombinators.ReadP (readP_to_S)

prelude :: Program
prelude = [
    ("zero", ([], Empty)),
    ("one", ([], Power Empty)),
    ("two", ([], Power (Power Empty))),
    ("pair", (["x", "y"], Repl "z" (If (Var "z") (Var "x") (Var "y"))
        (Const "two" []))),
    ("opair", (["x", "y"], Const "pair"
        [ Const "pair" [Var "x", Var "x"],
          Const "pair" [Var "x", Var "y"]])),
    ("succ", (["x"], Union (Const "pair" [Var "x", Const "pair" [Var "x", Var "x"]]))),
    ("Power", (["x"], Power (Var "x"))),
    ("Union", (["x"], Union (Var "x")))
    ]

main :: IO ()
main = do
    -- Read everything until EOF
    input <- getContents
    let res = readP_to_S program input
    case filter ((== "") . snd) res of
        [] -> putStrLn "No program found" >> print res
        ((prog,_):_) -> print(run (prog ++ prelude) [])
