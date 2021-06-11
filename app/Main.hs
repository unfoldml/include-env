{-# language TemplateHaskell #-}
module Main where

import IncludeEnv.TH (includeEnv)

$(includeEnv "SHELL" "shl")
shl :: String

main :: IO ()
main = putStrLn $ unwords ["your current shell :", shl] 


