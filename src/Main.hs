{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
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

import Interface.CLI (Opt (..), Format (..), parseOptions, autoparse)
import Parser.Parser (parseTPDB, parseTRS, parseTPDB_XML, parseTRS_XML, parseCOPS, parseTRS_COPS)

import System.IO (hPutStr, stdout)

import Parser.Grammar (Spec (..), Decl (..)) --import Parser.TPDB.Grammar (Spec (..),Decl(..))

--import Data.List(sort)

import System.Directory (doesDirectoryExist, getDirectoryContents, listDirectory)
import System.FilePath ((</>))

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
               , inputDir = dir
               , inputFormat = format } = opts

      existDir <- doesDirectoryExist dir
      if (existDir) then do
         --hPutStr stdout ("\n dir path:" ++ show dir ++ "\n")
         fileNms <- listDirectory dir -- FilePath -> IO [FilePath]
         --hPutStr stdout ("\n files paths:" ++ show fileNms ++ "\n")
         parseFiles dir (fileNms)
      else do
      
         filedata <- input
         
         -- >>>> Bloque para pruebas
         let !decls = case parseTRS filedata of --let !decls = case parseTRS filedata of
                                 Left parseerror
                                    -> error$ "Parse Error (Main): " ++ show parseerror
                                 --Right (Spec decl)
                                 Right decl
                                    -> decl

         hPutStr stdout ("\n Pruebas:\n"
                  ++ "+ Spec (decl list):\n  " ++ (show $ specToDecl decls) ++ "\n"
                  ++ "+ Longitud de la lista (num. de bloques):   " ++ (show $ length $ specToDecl decls) ++ "\n"
                  -- ++ "\nTiene var y rule: \n" ++ (show $ hasVar (specToDecl decls))
                        )
         -- <<<<
         --
         let !trs = case format of 
                  Just TPDB -> 
                        case parseTPDB filedata of
                           Left parseerror
                              -> error$ "Parse Error (Main): " ++ show parseerror
                           Right sys
                              -> "Success: " ++ show sys
                  Just XMLTPDB -> 
                        case parseTPDB_XML filedata of
                           Left parseerror
                              -> error$ "Parse Error (Main): " ++ show parseerror
                           Right sys
                              -> "Success: " ++ show sys
                  Just COPS -> 
                        case parseCOPS filedata of
                           Left parseerror
                              -> error$ "Parse Error (Main): " ++ show parseerror
                           Right sys
                              -> "Success: " ++ show sys
                  Nothing -> autoparse filename filedata

         hPutStr stdout ("\n TRS:\n" ++ show trs ++ "\n\n") --printOp spec
         --

{-
     --let !trs = autoparse filename filedata
     let !trs = case parseTPDB filedata of
                            Left parseerror
                               -> error$ "Parse Error (Main): " ++ show parseerror
                            Right sys
                              -> sys
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

-- | Parse all files in the given directory
parseFiles :: FilePath -> [FilePath] -> IO ()
parseFiles _ [] = hPutStr stdout(" ------------- END OF DIR -------------- ")
parseFiles dirPath (filep:rest) = do 
      let absPath= dirPath </> filep
      hPutStr stdout ("\n++ File:" ++ show absPath ++ " :\n")
      input <- readFile absPath -- readFile :: FilePath -> IO String
      let !trs = autoparse filep input  -- autoparse :: String -> String -> TRS
      hPutStr stdout (show trs ++ "\n")
      parseFiles dirPath rest

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