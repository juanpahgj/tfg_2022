{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This is the CSRS Syntax Checker Main Module
--
-----------------------------------------------------------------------------

module Main (

-- * Exported functions

main

) where

import Interface.CLI (Opt (..), Format (..), parseOptions) --, autoparse)
import Parser.TPDB.Parser (parseTPDB)

import System.IO (hPutStr, stdout)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | The 'main' function parses an input file and returns nothing if correct, an
-- error if incorrect
main :: IO ()
main =
  do (opts, _) <- parseOptions
     let Opt { inputName = filename 
             , inputContent = input
             , inputFormat = format } = opts
     filedata <- input
     let !trs = case parseTPDB filedata of
                            Left parseerror
                               -> error$ "Parse Error (Main): " ++ show parseerror
                            Right sys
                              -> sys
     hPutStr stdout (show trs) --- ""

     {-
     let !trs = case format of 
      Just TPDB -> 
              case parseTPDB filedata of
                Left parseerror
                   -> error$ "Parse Error (Main): " ++ show parseerror
                Right sys
                  -> sys
      Nothing -> autoparse filename filedata

      -}