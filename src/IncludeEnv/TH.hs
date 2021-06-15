{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
{-|
Include the value of an environment variable in the binary at compile time.

== Rationale
Users might want to embed secrets (e.g. API keys, database connection strings) inside production artifacts without checking these into the repository.

== Example

@
import IncludeEnv.TH (includeEnv)

$(`includeEnv` \"SHELL\" "shl")
shl :: String

main :: IO ()
main = putStrLn $ unwords ["your current shell :", shl]
@

-}
module IncludeEnv.TH (includeEnv) where

import System.Environment (lookupEnv)

-- template-haskell
import Language.Haskell.TH (runIO, runQ)
import Language.Haskell.TH.Syntax (Q, Exp(..), Dec(..), Pat(..), Name, mkName, Body(..), Lit(..))
import Language.Haskell.TH.Lib (valD)


-- | Include the value of an environment variable at compile time.
--
-- A fresh variable of type `String` is declared each time this is computation is evaluated.
--
-- Note : will crash with `error` if the environment variable is not found.
includeEnv :: String -- ^ name of environment variable to be looked up
           -> String -- ^ name of new value
           -> Q [Dec]
includeEnv e varname = do
  mstr <- runIO $ lookupEnv e
  case mstr of
    Just str -> decl varname str
    Nothing -> error $ unwords ["Cannot find variable", e, "in the environment."]
    where
      decl :: String -> String -> Q [Dec]
      decl n x = pure [dq] where
        dq = ValD qpat qbody []
        qpat = VarP (mkName n)
        qbody = NormalB (LitE (StringL x))



