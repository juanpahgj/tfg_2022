{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TPDB.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in the TPDB format.
--
-----------------------------------------------------------------------------

module Parser.TPDB.Parser (

-- * Exported functions

parseTPDB, parseTRS

)  where

import Parser.TPDB.TRS.Parser (trsParser)

import Parser.TPDB.TRS.Grammar (Spec (..), Decl (..), TRSType(..), TRS (..)
  , Term (..), Id, TRSType (..), Cond (..), Rule (..), getTerms)--, nonVarLHS, isCRule, hasExtraVars)

import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Text.ParserCombinators.Parsec.Error (Message (..), newErrorMessage)
import Text.Parsec.Pos (newPos)
import Data.Map as M (empty, lookup, insert)
import Data.Set as S (empty, fromList, member, union, intersection, null, elems)
import Data.List (sort, nub, intersperse)
import Control.Monad.State (State, evalState, get, put)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parses a TPDB problem and return a COPS Problem
parseTPDB :: String -> Either ParseError TRS
parseTPDB = checkConsistency . checkBlocks . parseTRS
--parseTPDB = checkConsistency . parseTRS 
--checkConsistency . parseTRS = \string -> checkConsistency(parseTRS(string))


-- | Parses a term rewriting system in TPDB format
parseTRS :: String -> Either ParseError Spec
parseTRS s = doParse s trsParser

-- | Parses the system an returns the parsing error or the succesful
-- parsed system.
doParse :: String -> Parser a -> Either ParseError a
doParse s p = parse p "" s
--(parse p filePath input) runs a character parser p without user state.
--The filePath is only used in error messages and may be the empty string.


-- | Must have at least one Var and one Rules block
checkBlocks :: Either ParseError Spec -> Either ParseError Spec -- 
checkBlocks (Left parseError) = Left parseError
checkBlocks (Right (Spec decls))= do{ if (hasVar decls) then 
                                        Right (Spec decls)
                                      else
                                        Left $ newErrorMessage (UnExpect $ "there is no required blocks") (newPos "" 0 0)
                                    }

hasVar :: [Decl] -> Bool
hasVar [] = False
hasVar (Var vs:rest)= hasRule rest
hasVar (d:ds) = hasVar ds
      
hasRule [] = False
hasRule (Rules r:_) = True
hasRule (d:ds) = hasRule ds


-- | Checks consistency (order, arguments and replacement map)
checkConsistency :: Either ParseError Spec -> Either ParseError TRS 
checkConsistency (Left parseError) = Left parseError
checkConsistency (Right (Spec decls)) 
  = evalState (checkWellFormed decls) (TRS M.empty S.empty [] TRSStandard) -- (TRS M.empty S.empty [] [] TRSStandard)

-- | Extracts the signature and checks if the rules are well-formed wrt that
-- signature. Precondition: Declarations are in order.
--
-- Must be sorted. 
checkWellFormed :: [Decl] -> State TRS (Either ParseError TRS)
checkWellFormed [] = do { myTRS <- get 
                        ; return . Right $ myTRS}
  

--checkWellFormed (Var vs:rest) = checkWellFormed rest

checkWellFormed ((Var vs):rest) = do { myTRS <- get
                                   ; let vars = trsVariables myTRS -- Set Char
                                   ; let vsSet = S.fromList vs -- Set Char
                                   ; let duplicated = S.intersection vsSet vars
                                   ; if (not . S.null $ duplicated) then
                                      return . Left $ newErrorMessage (UnExpect $ "variable(s) already declared: " ++ (concat . intersperse ", " . S.elems $ duplicated)) (newPos "" 0 0)

                                   else
                                     do{ put $ myTRS { trsVariables = S.union vars vsSet }
                                       ; checkWellFormed rest
                                       }
                                   }

checkWellFormed (Theory th:rest) = checkWellFormed rest
--checkWellFormed ((Theory th):rest) = checkWellFormed rest

--checkWellFormed (Rules rs:rest) = checkWellFormed rest
-- checkWellFormed ((Rules rs):rest) = checkWellFormed rest

checkWellFormed (Rules rs:rest) = do { result <- checkRules rs
                                     ; case result of
                                         Left parseError -> return . Left $ parseError
                                         Right _ -> do { myTRS <- get
                                                       ; put $ myTRS {trsRules = rs}
                                                       ; checkWellFormed rest
                                                       }
                                     }


-- | Checks if the rules are well-formed wrt the extracted signature
checkRules :: [Rule] -> State TRS (Either ParseError ())
checkRules [] = do { myTRS <- get
                   ; return . Right $ ()
                      -- first, we extract the arity of symbols, then we check RMap 
                    --; case trsType myTRS of
                    --  TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                    --  TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                    -- _ -> return . Right $ ()
                    }
checkRules (r:rs) = do { myTRS <- get
                        ; let vs = trsVariables myTRS
                        --; if nonVarLHS vs r then -- lhs is non-variable 
                            --if isCRule r || (not . hasExtraVars vs $ r) then -- extra variables not allowed in non-conditional rules
                        ; do { result <- checkTerms . getTerms $ r 
                            ; case result of
                                Left parseError -> return . Left $ parseError 
                                Right _ -> checkRules rs
                            }
                            --else
                            --return . Left $ newErrorMessage (UnExpect $ "extra variables in the rule " ++ (show r)) (newPos "" 0 0)
                          --else
                            --return . Left $ newErrorMessage (UnExpect $ "variable in the left-hand side of the rule " ++ (show r)) (newPos "" 0 0)
                        }


-- | Checks if the terms are well-formed wrt the extracted signature
checkTerms :: [Term] -> State TRS (Either ParseError ())
checkTerms [] = return . Right $ ()
checkTerms (t:ts) = do { result <- checkTerm t 
                        ; case result of
                            Left parseError -> return . Left $ parseError
                            Right _ -> checkTerms ts
                        }

-- | Checks if the term is well-formed wrt the extracted signature
checkTerm :: Term -> State TRS (Either ParseError ())
checkTerm (T id terms) = do { myTRS <- get
                            ; let vars = trsVariables myTRS
                            ; let funcs = trsSignature myTRS
                            ; let arglen = length terms
                            ; case (S.member id vars, M.lookup id funcs) of 
                                (False, Nothing) -> do { put $ myTRS { trsSignature = M.insert id (length terms) $ funcs }
                                                      ; checkTerms terms
                                                      }
                                (False, Just len) -> if (arglen == len) then 
                                                      checkTerms terms 
                                                    else
                                                      return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arglen) ++ " in term " ++ (show $ T id terms)) (newPos "" 0 0)
                                (True, Nothing) -> if (arglen == 0) then 
                                                    return . Right $ ()
                                                  else
                                                    return . Left $ newErrorMessage (UnExpect $ "arguments in variable " ++ id) (newPos "" 0 0)
                                -- next case is not possible
                                _ -> return . Left $ newErrorMessage (UnExpect $ "variable and function symbols declaration " ++ id) (newPos "" 0 0)
                            }
                            

{-
checkWellFormed (CType SemiEquational:rest) = do { myTRS <- get 
                                                 ; put $ myTRS { trsType = TRSConditional SemiEquational }
                                                 ; checkWellFormed rest
                                                 }
checkWellFormed (CType Join:rest) = do { myTRS <- get 
                                       ; put $ myTRS { trsType = TRSConditional Join }
                                       ; checkWellFormed rest
                                       }
checkWellFormed (CType Oriented:rest) = do { myTRS <- get 
                                           ; put $ myTRS { trsType = TRSConditional Oriented }
                                           ; checkWellFormed rest
                                           }

-}



{-
checkWellFormed (Context rmap:rest) = do { myTRS <- get 
                                         ; if (length . nub . map fst $ rmap) == length rmap then
                                             do { put $ myTRS { trsRMap = rmap
                                                              , trsType 
                                                                  = case trsType myTRS of
                                                                      TRSStandard -> TRSContextSensitive
                                                                      TRSConditional typ -> TRSContextSensitiveConditional typ
                                                              }
                                                ; checkWellFormed rest
                                                }
                                           else 
                                             return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in replacement map declaration") (newPos "" 0 0)
                                         }

-}

{-


-- | Checks if the replacement map satisfies arity restriction and increasing order
checkRMap :: [(Id, [Int])] -> State TRS (Either ParseError ())
checkRMap [] = return . Right $ ()
checkRMap ((f,[]):rmaps) = do { myTRS <- get 
                              ; case M.lookup f (trsSignature myTRS) of 
                                  Nothing -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in rules)") (newPos "" 0 0)
                                  Just arity -> checkRMap rmaps 
                              }
checkRMap ((f,rmap):rmaps) = do { myTRS <- get 
                                ; case M.lookup f (trsSignature myTRS) of 
                                   Nothing -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in rules)") (newPos "" 0 0)
                                   Just arity -> let srmap = sort rmap in
                                                 if (rmap == srmap) && (head rmap >= 1) && (last rmap <= arity) then
                                                   checkRMap rmaps 
                                                 else
                                                   return . Left $ newErrorMessage (UnExpect $ "replacement map for symbol " ++ f ++ " (must be empty" ++ (if arity > 0 then " or an ordered list of numbers in [1.." ++ (show arity) ++ "] separated by commas" else "") ++ ")") (newPos "" 0 0) 
                                }

-}