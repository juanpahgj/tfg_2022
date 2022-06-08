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
import Parser.TPDB.Parser (parseTPDB, parseTRS)

import System.IO (hPutStr, stdout)

import Parser.TPDB.Grammar (Spec (..), Decl (..)) --import Parser.TPDB.TRS.Grammar (Spec (..),Decl(..))

--import Data.List(sort)

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
     --
     let !decls = case parseTRS filedata of
                            Left parseerror
                               -> error$ "Parse Error (Main): " ++ show parseerror
                            --Right (Spec decl)
                            Right decl
                               -> decl
     --
     let !trs = case parseTPDB filedata of
                            Left parseerror
                               -> error$ "Parse Error (Main): " ++ show parseerror
                            Right sys
                              -> sys
     hPutStr stdout ("Spec (decl list):\n\n" ++ (show $ specToDecl decls) ++ "\n---------\n" ++ "TRS:\n\n" ++ show trs)--printOp spec
     hPutStr stdout ("\n\nPruebas:\n"
                     ++ "longitud de la lista (num. de bloques): " ++ (show $ length $ specToDecl decls) 
                     -- ++ "\nTiene var y rule: \n" ++ (show $ hasVar (specToDecl decls))
                    )
     --hPutStr stdout (show trs) --- ""

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

--- Aux. fun.
specToDecl :: Spec -> [Decl]
specToDecl (Spec decls) = decls --specToDecl (Spec decls) = sort decls

hasVar :: [Decl] -> Bool
hasVar [] = False
hasVar (Var vs:rest)= hasRule rest
hasVar (d:ds) = hasVar ds
      
hasRule [] = False
hasRule (Rules r:_) = True
hasRule (d:ds) = hasRule ds

{-

--hasVarRule= (Left parseError) = Left parseError
hasVarRule (Spec decls) = do {let arglen = length decls
                                     ; case (S.member id vars) of
                                        (True) -> if (arglen == 0) then 
                                                            return . Right $ ()
                                                           else
                                                            return . Left $ newErrorMessage (UnExpect $ "arguments in variable " ++ id) (newPos "" 0 0)

                                        _ -> return . Left $ newErrorMessage (UnExpect $ "variable and function symbols declaration " ++ id) (newPos "" 0 0)
                              }
-}

{-
--------------------Borrar, imprimir decl
showOp :: [Decl] -> String
showOp [] = [] -- the empty list is a String
showOp (o:os) = showo ++ ('\n' : showos)
   where showo = (show o) -- this is a String, i.e. [Char]
         showos = showOp os -- this is also a String

printOp :: [Decl] -> IO ()
printOp xs = putStr $ showOp xs
----------------------
-}