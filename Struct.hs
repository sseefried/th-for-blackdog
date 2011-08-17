{-# LANGUAGE TemplateHaskell #-}
module Struct where

import Language.Haskell.TH
import Data.Maybe

data Struct = Arr [Struct]
            | Dict [(String,Struct)]
            | Number Int
            | Flt Float
            | Str String

getCons :: Q [Con]
getCons = do
  info <- reify ''Struct
  case info of
    TyConI (DataD _ _ _ cons _) -> return cons
    _ -> return [] -- or throw an error

--
-- A more detailed example where I turn constructors into
-- patterns with one or more wildcard patterns in them. e.g.
-- 
-- @Str String@ becomes pattern @(Str _)@
-- @A Int Float@ becomes pattern @(A _ _)@
--
consToPats :: Q [Pat]
consToPats = do
  info <- reify ''Struct
  case info of
    TyConI (DataD _ _ _ cons _) -> do
       pats <- mapM go cons
       return $ catMaybes pats
    _ -> return []
  where
    go :: Con -> Q (Maybe Pat)
    go (NormalC conName typs) = do
        pat <- conP conName (take (length typs) $ repeat wildP)
        return $ Just pat
    go _ = return Nothing

