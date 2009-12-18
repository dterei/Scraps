-- Tower of Hanoi Solver
-- Author: David Terei
--
-- This program will solve the tower of hanoi problem for you.
-- It can be run in two modes, one which will print out all the
-- steps that need to be made for the solution and a second mode
-- which doesn't print out anything, useful for benchmarking the
-- algorithms performance.
--
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Flag
    = Help
    | Benchmark
    deriving (Eq, Ord, Show, Enum, Bounded)

data Pole  = A | B | C deriving (Eq, Show)
type StackItem = Int
type Stack = [StackItem]
type SolnPrinter = Int -> Pole -> Pole -> [String]

flags =
    [ Option ['h'] ["help"] (NoArg Help) "Print this help message."
    , Option ['b'] ["benchmark"] (NoArg Benchmark) "Solve without printing the solution."
    ]

main :: IO ()
main = getArgs >>= parse

parse argv = case getOpt Permute flags argv of
    (args,[num],[]) -> do
        let n = read num
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else if Benchmark `elem` args
                then putStrLn . show . length $ solveTower n benchSoln
                else mapM_ putStrLn $ solveTower n printSoln

    (_, x:xs, []) -> do
        hPutStrLn stderr ("More then one tower size specified!\n\n" ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where
        header = "Usage: tower [-hb] <height of tower>"

solveTower :: Int -> SolnPrinter -> [String]
solveTower n p | n < 1     = error "Must be a tower of size 1 or greater"
               | otherwise = tower ([n,(n-1)..1]) A C p

tower :: Stack -> Pole -> Pole -> SolnPrinter -> [String]
tower (x:[]) on to p = p x on to
tower xs on to p =
    let spair = head $ [A,B,C] \\ [on,to]
    in tower (tail xs) on spair p
        ++ tower ([head xs]) on to p
        ++ tower (tail xs) spair to p

printSoln :: SolnPrinter
printSoln x on to = ["Move S" ++ (show x) ++ " from " ++ (show on) ++ " to " ++ (show to)]

benchSoln :: SolnPrinter
benchSoln _ _ _ = []

