{-# language TemplateHaskell #-}
module Main where

import IncludeEnv.TH (includeEnv, includeEnvLenient)


main :: IO ()
main = putStrLn "hello!"


$(includeEnv "SHELL" "shl")
shl :: String

$(includeEnvLenient "POTATO" "potato")
potato :: String
