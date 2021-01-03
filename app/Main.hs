module Main where

import qualified Momentum (query)
import qualified SPFive (query)
import System.Exit (die)

type StrategyList = (String, (String, IO ()))

main :: IO ()
main = do
  let loop = do
        let strategyList = listStrategy
        printStrategy strategyList
        cmd <- getLine
        case cmd of
          "q" -> die "Exiting....."
          _ -> case lookup cmd strategyList of
            Just v -> snd v
            Nothing -> putStrLn "Not within the function call"
        loop
  loop
  where
    listStrategy :: [StrategyList]
    listStrategy =
      zip
        (map show [1 ..])
        [ ("Equal-Weight S&P 500", SPFive.query),
          ("Momentum Investing Strategy", Momentum.query)
        ]
    printStrategy :: [StrategyList] -> IO ()
    printStrategy strategies =
      do
        putStrLn "List of strategies:"
        mapM_ (\(x, (y, _)) -> putStrLn $ x ++ ": " ++ y) strategies
        putStrLn "q: To quit gracefully"
        putStrLn "-----------"
