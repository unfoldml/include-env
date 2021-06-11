{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
{-|
Include the value of an environment variable at compile time

== Rationale
The first use case for this library is to embed secrets (e.g. API keys) inside production artifacts without checking them into the repository.

-}
module IncludeEnv.TH (includeEnv) where

import System.Environment (lookupEnv)

-- template-haskell
import Language.Haskell.TH (runIO, runQ)
import Language.Haskell.TH.Syntax (Q, Exp(..), Dec(..), Pat(..), Name, mkName, Body(..), Lit(..))
import Language.Haskell.TH.Lib (valD)


-- | Include the value of an environment variable at compile time
--
-- A fresh variable is declared each time this is computation is evaluated
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
      decl n x = fmap (:[]) $ pure $ ValD qpat qbody [] where
        qpat = VarP (mkName n)
        qbody = NormalB (LitE (StringL x))



