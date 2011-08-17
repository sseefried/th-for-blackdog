{-# LANGUAGE TemplateHaskell #-}
module Jcheck where

-- standard libraries
import Language.Haskell.TH

-- friends
import Struct


--
-- So, why does this have to be spliced in this module and not in Struct?
--
-- You get a staging error from GHC if you try to do it in module Struct
--
-- GHC stage restriction: `getCons'
--     is used in a top-level splice or annotation,
--     and must be imported, not defined locally
--   In the second argument of `(=<<)', namely `getCons'
--   In the expression: stringE . show =<< getCons
--   In the expression: $(stringE . show =<< getCons)
--
-- Reason? Actually, pretty obvious when you think about it:
--
-- When you splice something you're running functions at compile time.
-- To run @getCons@ it needs to have been compiled already.
-- Therefore it can't be in the same module as the one you're running the splice (at compile-time) in.
--

structConsString = $(stringE . show =<< getCons )

structConsToPats = $(stringE . show =<< consToPats)
