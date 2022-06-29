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

import Interface.CLI (Opt (..), Format (..), parseOptions)--, autoparse)
import Parser.Parser (parseTPDB, parseTRS, parseTPDB_XML, parseTRS_XML, parseCOPS, parseTRS_COPS)
import Parser.Grammar (Spec (..), Decl (..), TRS)
import Text.ParserCombinators.Parsec.Error(ParseError)

import System.IO (hPutStr, stdout)
import Control.Monad (msum, MonadPlus (..))
import Data.List (isSuffixOf)
--import Data.List(sort)

import System.Directory (doesDirectoryExist, getDirectoryContents, listDirectory)
import System.FilePath ((</>))

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | The 'main' function parses an input file or dir and returns nothing if correct, an
-- error if incorrect
main :: IO ()
main =
   do (opts, _) <- parseOptions
      let Opt { inputName = filename 
               , inputContent = input
               , inputDir = dir
               , inputFormat = format } = opts

      existDir <- doesDirectoryExist dir
      if (existDir) then 
         do {dirPaths <- listDirectory dir -- FilePath -> IO [FilePath]
            ;parseFiles dir (dirPaths) format
            }
      else
         do{filedata <- input
           ;let !trs = callParse filename filedata format
           ;hPutStr stdout ("\n Success:\n" ++ show trs ++ "\n\n")
           }



-- | Parse all files in the given directory
parseFiles :: FilePath -> [FilePath] -> Maybe Format -> IO ()
parseFiles _ [] _ = hPutStr stdout (" ------------- END OF DIR ------------- ")
parseFiles dirPath (filepath:rest) format= do 
      if ((toParse filepath aivailableFormats)) then
         do {let absPath= dirPath </> filepath
            ;input <- readFile absPath -- readFile :: FilePath -> IO String
            ;hPutStr stdout ("\n++ File:" ++ show absPath ++ " :\n")
            ;let !trs = callParse filepath input format
            ;hPutStr stdout ("Success: " ++ show trs ++ "\n")
            
            --Write results
            ;let trsOut = ("\n++ File:" ++ show absPath ++ " :\n" ++ show trs ++ "\n")
            ;let writePath = dirPath </> "parser_results.txt"
            ;appendFile writePath trsOut
            ;parseFiles dirPath rest format
            }
      else
         parseFiles dirPath rest format
      

toParse filename [] = False
toParse filename ((ext,_):xs) | ext `isSuffixOf` filename = True
                              | otherwise = toParse filename xs

callParse :: String -> String -> Maybe Format -> TRS
callParse filename filedata format= do
   case format of 
            Just TPDB -> 
                  case parseTPDB filedata of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> sys
            Just XMLTPDB -> 
                  case parseTPDB_XML filedata of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> sys
            Just COPS -> 
                  case parseCOPS filedata of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> sys
            Nothing -> autoparse filename filedata


-- | File extensions
aivailableFormats :: [(String, String -> Either ParseError TRS)]
aivailableFormats = [(".trs", parseTPDB), (".xml", parseTPDB_XML), (".trs", parseCOPS)]

-- | Parse file into a TRS
autoparse :: String -> String -> TRS
autoparse fname = maybe (error "Error (CLI): File Extension not supported")
                        parseWithFailure
                        matchParser
   where matchParser
             = msum $ map (\(ext,p)-> if fname `endsWith` ext then Just p else Nothing) aivailableFormats
         endsWith
             = flip isSuffixOf
         parseWithFailure parser contents
             = case parser contents of
                 Left parseerror
                     -> error$ "Parse Error (CLI): " ++ show parseerror
                 Right sys
                     -> sys

{-
-- | Parse file
anyParse :: String -> TRS  --String
anyParse fdata = checkParser $ map (\(_, p) -> p fdata) aivailableFormats

checkParser [] = (error "Error (CLI): Format not supported") 
checkParser ((Right x):_) = x --"Success: " ++ show x
checkParser ((Left _):xs) = checkParser xs
-}



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
{-
     --let !trs = autoparse filename filedata
     let !trs = case parseTPDB filedata of
                            Left parseerror
                               -> error$ "Parse Error (Main): " ++ show parseerror
                            Right sys
                              -> sys
-}






   {-
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
         -}
--