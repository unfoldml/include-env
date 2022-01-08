{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
{-|
Include the value of an environment variable in the binary at compile time.

== Rationale
Users might want to embed secrets (e.g. API keys, database connection strings) inside production artifacts without checking these into the repository.

== Examples

NB : all library functions require the `TemplateHaskell` language extension.

=== Include a single variable

In this case, the name of the user's current shell) :

@
import IncludeEnv.TH (includeEnv)

$(`includeEnv` \"SHELL\" "shl")
shl :: String

main :: IO ()
main = putStrLn $ unwords ["your current shell :", shl]
@

=== Include a group of variables as a name-value map

@
import IncludeEnv.TH (includeEnvMap)

env = $(`includeEnvMap` [\"TERM\", \"USER\"])
@

@
>>> env
fromList [(\"TERM\","dumb"),(\"USER\","marco")]
@

-}
module IncludeEnv.TH (
  includeEnv
  , includeEnvLenient
  , includeEnvMaybe
  -- * containers
  , includeEnvMap
  ) where

import Control.Monad (foldM)

import System.Environment (lookupEnv)

-- containers
import qualified Data.Map.Strict as M (Map, lookup, insert, fromList)
-- template-haskell
import Language.Haskell.TH (runIO, runQ)
import Language.Haskell.TH.Syntax (Q, Exp(..), Dec(..), Pat(..), Name, mkName, Body(..), Lit(..), reportWarning)
import Language.Haskell.TH.Lib (valD)
-- th-lift-instances
import Instances.TH.Lift


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

-- | Like 'includeEnv' but only prints a warning if the environment variable cannot be found.
--
-- NB : If the lookup fails, the declared value will contain an _empty string_ .
includeEnvLenient :: String -- ^ name of environment variable to be looked up
                  -> String -- ^ name of new value
                  -> Q [Dec]
includeEnvLenient e varname = do
  mstr <- runIO $ lookupEnv e
  case mstr of
    Just str -> decl varname str
    Nothing -> do
      reportWarning $ unwords ["*** WARNING : Cannot find variable", e, "in the environment."]
      decl varname ""
    where
      decl :: String -> String -> Q [Dec]
      decl n x = pure [dq] where
        dq = ValD qpat qbody []
        qpat = VarP (mkName n)
        qbody = NormalB (LitE (StringL x))

-- | Like 'includeEnv' but produces a 'Maybe String'
--
-- Use case : The program needs to be compiled against two different environments that may have different sets of environment variables. 'includeEnvMaybe' lets you account for the results of multiple such lookups at runtime.
--
-- @since 0.4.0.0
includeEnvMaybe :: String -- ^ name of environment variable to be looked up
                -> Q Exp
includeEnvMaybe e = do
  mstr <- runIO $ lookupEnv e
  [| mstr |]

-- | Lookup a number of environment variables and populate a 'M.Map' with the result
--
-- NB: if a variable name cannot be found, the corresponding entry will be missing
--
-- @since 0.5.0.0
includeEnvMap :: Foldable t =>
                 t String -- ^ names of environment variable to be looked up
              -> Q Exp
includeEnvMap es = do
  mm <- foldM insf mempty es
  [| mm |]
  where
    insf acc k = do
      mv <- runIO $ lookupEnv k
      case mv of
        Nothing -> pure acc
        Just v -> pure $ M.insert k v acc
