module Main where

import qualified HCat (runHCat, runHCat7)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  HCat.runHCat >> HCat.runHCat7
