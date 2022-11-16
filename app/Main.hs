module Main where

import qualified HCat (runHCat)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  HCat.runHCat
