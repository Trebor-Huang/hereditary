module Main where

import SetFuck
import Parser
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Environment (getArgs)

usage :: String
usage = unlines [
    "Usage:",
    "  setfuck [file]",
    "Interpretes the file as a setfuck program.",
    "If no file is given, reads on stdin."]

prelude :: Program
prelude = [
    ("Empty", ([], Empty)),
    ("Power", (["x"], Power (Var "x"))),
    ("Union", (["x"], Union (Var "x"))),
    ("Pair", (["x", "y"], Pair (Var "x") (Var "y")))
    ]

main :: IO ()
main = do
    args <- getArgs
    input <- if null args then do
        putStrLn usage
        getContents
    else do
        readFile (head args)
    let res = readP_to_S program input
    case filter ((== "") . snd) res of
        [] -> putStrLn "No program found" >> print res
        ((prog,_):_) -> print(run (prog ++ prelude) [])
