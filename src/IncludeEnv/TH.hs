{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
module IncludeEnv.TH (includeEnv) where

import System.Environment (lookupEnv)

-- template-haskell
import Language.Haskell.TH (runIO, runQ)
import Language.Haskell.TH.Syntax (Q, Exp(..), Dec(..), Pat(..), Name, mkName, Body(..), Lit(..))
import Language.Haskell.TH.Lib (valD)


-- | Include the value of an environment variable inside the 
includeEnv :: String -- ^ name of environment variable
           -> String -- ^ name of value in compiled code
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



