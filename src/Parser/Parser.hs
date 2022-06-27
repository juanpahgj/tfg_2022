{-# LANGUAGE FlexibleContexts #-}

module Parser.Parser (

-- * Exported functions

parseTPDB, parseTPDB_XML, parseTRS, parseTRS_XML, parseCOPS, parseTRS_COPS

)  where

import Parser.TPDB.TRS.Parser (trsParser)
import Parser.TPDB.TRS_XML.Parser (trsXmlParser)
import Parser.COPS.TRS.Parser (trsCOPSParser)

import Parser.Grammar (Spec (..), Decl (..), TRSType(..), TRS (..), Term (..), XmlTerm (..)
  , Id, TRSType (..), Cond (..), Rule (..), CondType (..), Strategydecl (..), Signdecl (..)
  , getTerms, nonVarLHS, isCRule, hasExtraVars)

import Text.ParserCombinators.Parsec (parse, Parser, ParseError)
import Text.ParserCombinators.Parsec.Error (Message (..), newErrorMessage)
import Text.Parsec.Pos (newPos)
import Data.Map as M (empty, lookup, insert)
import Data.Set as S (empty, fromList, member, union, intersection, null, elems, insert)
import Data.List (sort, nub, intersperse)
import Control.Monad.State (State, evalState, get, put)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Parses a TPDB problem
parseTPDB :: String -> Either ParseError TRS
parseTPDB = (checkConsistency (checkTPDBDeclaration)) . parseTRS -- . checkSortBlocks . parseTRS
--parseTPDB = checkConsistencyWithoutcheckDcl . parseTRS
--parseTPDB = checkConsistency . parseTRS 

-- | Parses a term rewriting system in TPDB format
parseTRS :: String -> Either ParseError Spec
parseTRS s = sortSpec (doParse s trsParser)

-----------------------------------------------------------------------------

-- | Parses a TPDB-XML problem
parseTPDB_XML :: String -> Either ParseError TRS
parseTPDB_XML = (checkConsistency (checkXMLDeclaration)) . parseTRS_XML

-- | Parses a term rewriting system in TPDB format
parseTRS_XML :: String -> Either ParseError Spec
parseTRS_XML s = sortSpec (doParse s trsXmlParser)

-----------------------------------------------------------------------------

-- | Parses a COPS problem
parseCOPS :: String -> Either ParseError TRS
parseCOPS = (checkConsistency (checkCOPSDeclaration)) . parseTRS_COPS

-- | Parses a term rewriting system in COPS format
parseTRS_COPS :: String -> Either ParseError Spec
parseTRS_COPS s = sortSpec (doParse s trsCOPSParser)

-----------------------------------------------------------------------------

-- | Parses the system and returns the parsing error or the succesful
-- parsed system.
doParse :: String -> Parser a -> Either ParseError a
doParse s p = parse p "" s
--(parse p filePath input) runs a character parser p without user state.
--The filePath is only used in error messages and may be the empty string.


sortSpec :: Either ParseError Spec -> Either ParseError Spec
sortSpec (Left parseError) = Left parseError
sortSpec (Right (Spec decls))=  Right (Spec $ sort decls)

{-
  -- | Must have at least one Var and one Rules block
  checkSortBlocks :: Either ParseError Spec -> Either ParseError Spec
  checkSortBlocks (Left parseError) = Left parseError
  checkSortBlocks (Right (Spec decls))= do{ let sortedDecls = sort decls
                                          ;  if (hasVar sortedDecls) then 
                                              Right (Spec sortedDecls)
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
-}


-- | Checks declaration order for TPDB
checkTPDBDeclaration :: Either ParseError Spec -> Decl -> Either ParseError Spec
checkTPDBDeclaration (Left parseError) _ = Left parseError
checkTPDBDeclaration (Right (Spec [])) (Var vs) = Right . Spec $ [Var vs]
checkTPDBDeclaration (Right (Spec (Var vs:rest))) (Var vss) = Right . Spec $ (Var vss:Var vs:rest)
checkTPDBDeclaration (Right (Spec (Var vs:rest))) (Strategy st) = Right . Spec $ [Strategy st, Var vs]
checkTPDBDeclaration (Right (Spec (Var vs:rest))) (Theory th) = Right . Spec $ (Theory th:Var vs:rest)
checkTPDBDeclaration (Right (Spec (Var vs:rest))) (Rules rs) = Right . Spec $ (Rules rs:Var vs:rest)
checkTPDBDeclaration (Right (Spec (Strategy st:rest))) (Theory th) = Right . Spec $ (Theory th:Strategy st:rest)
checkTPDBDeclaration (Right (Spec (Strategy st:rest))) (Rules rs) = Right . Spec $ (Rules rs:Strategy st:rest)
checkTPDBDeclaration (Right (Spec (Theory th:rest))) (Theory thh) = Right . Spec $ (Theory thh:Theory th:rest)
checkTPDBDeclaration (Right (Spec (Theory th:rest))) (Rules rs) = Right . Spec $ (Rules rs:Theory th:rest)
checkTPDBDeclaration (Right (Spec (Rules rs:rest))) (Rules rss) = Right . Spec $ (Rules rss:Rules rs:rest)
checkTPDBDeclaration (Right (Spec (Rules rs:rest))) (AnyList id al) = Right . Spec $ ((AnyList id al):Rules rs:rest)
checkTPDBDeclaration (Right (Spec ((AnyList id al):rest))) (AnyList idd all) = Right . Spec $ ((AnyList idd all):(AnyList id al):rest)
checkTPDBDeclaration _ (Var _) = Left $ newErrorMessage (UnExpect "VAR block") (newPos "" 0 0)
checkTPDBDeclaration _ (Rules _) = Left $ newErrorMessage (UnExpect "RULES block") (newPos "" 0 0)
checkTPDBDeclaration _ (Theory _) = Left $ newErrorMessage (UnExpect "THEORY block") (newPos "" 0 0)
checkTPDBDeclaration _ (Strategy _) = Left $ newErrorMessage (UnExpect "STRATEGY block") (newPos "" 0 0)
checkTPDBDeclaration _ (AnyList _ _) = Left $ newErrorMessage (UnExpect "ANYLIST block") (newPos "" 0 0)

-- | Checks declaration order for TPDB xml
checkXMLDeclaration :: Either ParseError Spec -> Decl -> Either ParseError Spec
checkXMLDeclaration (Left parseError) _ = Left parseError
checkXMLDeclaration (Right (Spec [])) (CType ctype) = Right . Spec $ [CType ctype]
checkXMLDeclaration (Right (Spec [])) (Strategy stgy) = Right . Spec $ [Strategy stgy]
checkXMLDeclaration (Right (Spec [CType ctype])) (Strategy stgy) = Right . Spec $ [Strategy stgy, CType ctype]
checkXMLDeclaration (Right (Spec (Strategy stgy:rest))) (Signature sg) = Right . Spec $ (Signature sg:Strategy stgy:rest)
checkXMLDeclaration (Right (Spec (Signature sg:rest))) (Rules rs) = Right . Spec $ (Rules rs:Signature sg:rest)
checkXMLDeclaration (Right (Spec (Rules rs:rest))) (Rules rss) = Right . Spec $ (Rules rss:Rules rs:rest)
checkXMLDeclaration (Right (Spec (Rules rs:rest))) (Comment c) = Right . Spec $ (Comment c:Rules rs:rest)
checkXMLDeclaration _ (CType _) = Left $ newErrorMessage (UnExpect "CONDITIONTYPE block") (newPos "" 0 0)
checkXMLDeclaration _ (Strategy _) = Left $ newErrorMessage (UnExpect "STRATEGY block") (newPos "" 0 0)
checkXMLDeclaration _ (Rules _) = Left $ newErrorMessage (UnExpect "RULES block") (newPos "" 0 0)
checkXMLDeclaration _ (Signature _) = Left $ newErrorMessage (UnExpect "SIGNATURE block") (newPos "" 0 0)
checkXMLDeclaration _ (Comment _) = Left $ newErrorMessage (UnExpect "COMMENT block") (newPos "" 0 0)

-- | Checks declaration order for COPS
checkCOPSDeclaration :: Either ParseError Spec -> Decl -> Either ParseError Spec
checkCOPSDeclaration (Left parseError) _ = Left parseError
checkCOPSDeclaration (Right (Spec [])) (Var vs) = Right . Spec $ [Var vs]
checkCOPSDeclaration (Right (Spec [])) (CType ctype) = Right . Spec $ [CType ctype]
checkCOPSDeclaration (Right (Spec [])) (Context rmap) = Right . Spec $ [Context rmap]
checkCOPSDeclaration (Right (Spec [])) (Signature sg) = Right . Spec $ [Signature sg]
checkCOPSDeclaration (Right (Spec [])) (Rules rs) = Right . Spec $ [Rules rs]
checkCOPSDeclaration (Right (Spec (Var vs:rest))) (Var vss) = Right . Spec $ (Var vss:Var vs:rest)
checkCOPSDeclaration (Right (Spec (Var vs:rest))) (CType ctype) = Right . Spec $ (CType ctype:Var vs:rest)
checkCOPSDeclaration (Right (Spec (Var vs:rest))) (Context rmap) = Right . Spec $ (Context rmap:Var vs:rest)
checkCOPSDeclaration (Right (Spec (Var vs:rest))) (Signature sg) = Right . Spec $ (Signature sg:Var vs:rest)
checkCOPSDeclaration (Right (Spec (Var vs:rest))) (Rules rs) = Right . Spec $ (Rules rs:Var vs:rest)
checkCOPSDeclaration (Right (Spec (CType ctype:rest))) (Var vs) = Right . Spec $ (Var vs:CType ctype:rest)
checkCOPSDeclaration (Right (Spec (CType ctype:rest))) (Context rmap) = Right . Spec $ (Context rmap:CType ctype:rest)
checkCOPSDeclaration (Right (Spec (CType ctype:rest))) (Rules rs) = Right . Spec $ (Rules rs:CType ctype:rest)
checkCOPSDeclaration (Right (Spec (Context rmap:rest))) (Rules rs) = Right . Spec $ (Rules rs:Context rmap:rest)
checkCOPSDeclaration (Right (Spec (Signature sg:rest))) (Rules rs) = Right . Spec $ (Rules rs:Signature sg:rest)
checkCOPSDeclaration (Right (Spec (Rules rs:rest))) (Rules rss) = Right . Spec $ (Rules rss:Rules rs:rest)
checkCOPSDeclaration (Right (Spec (Rules rs:rest))) (Comment c) = Right . Spec $ (Comment c:Rules rs:rest)
checkCOPSDeclaration _ (CType _) = Left $ newErrorMessage (UnExpect "CONDITIONTYPE block") (newPos "" 0 0)
checkCOPSDeclaration _ (Var _) = Left $ newErrorMessage (UnExpect "VAR block") (newPos "" 0 0)
checkCOPSDeclaration _ (Context _) = Left $ newErrorMessage (UnExpect "REPLACEMENT-MAP block") (newPos "" 0 0)
checkCOPSDeclaration _ (Rules _) = Left $ newErrorMessage (UnExpect "RULES block") (newPos "" 0 0)
checkCOPSDeclaration _ (Signature _) = Left $ newErrorMessage (UnExpect "SIGNATURE block") (newPos "" 0 0)
checkCOPSDeclaration _ (Comment _) = Left $ newErrorMessage (UnExpect "COMMENT block") (newPos "" 0 0)


-- | Checks consistency (order, arguments and replacement map)
checkConsistency :: (Either ParseError Spec -> Decl -> Either ParseError Spec) -> Either ParseError Spec -> Either ParseError TRS 
checkConsistency _ (Left parseError) = Left parseError
checkConsistency checkDclFun (Right (Spec decls)) 
  = case foldl checkDclFun (Right (Spec [])) decls of
        Left parseError -> Left parseError
        Right (Spec _) -> evalState (checkWellFormed decls) (TRS M.empty S.empty [] [] TRSStandard Nothing False) -- (TRS M.empty S.empty [] [] TRSStandard)
{-
  ---- >>> borrar >>>
  checkConsistencyWithoutcheckDcl (Right (Spec decls)) 
    = evalState (checkWellFormed decls) (TRS M.empty S.empty [] [] TRSStandard Nothing) -- (TRS M.empty S.empty [] [] TRSStandard)
  -- <<<<<<<<<<
-}

-- | Extracts the signature and checks if the rules are well-formed wrt that
-- signature. Precondition: Declarations are in order.
--
-- Must be sorted. 
checkWellFormed :: [Decl] -> State TRS (Either ParseError TRS)
checkWellFormed [] = do { myTRS <- get 
                        ; return . Right $ myTRS}

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

checkWellFormed ((Rules rs):rest) = do {result <- checkRules rs
                                       ; case result of
                                           Left parseError -> return . Left $ parseError
                                           Right _ -> do { myTRS <- get
                                                         ; let rules = trsRules myTRS
                                                         ; put $ myTRS {trsRules = (rs ++ rules) }
                                                         ; checkWellFormed rest
                                                         }
                                       }

checkWellFormed (CType SEMIEQUATIONAL:rest) = do { myTRS <- get 
                                                 ; put $ myTRS { trsType = TRSConditional SEMIEQUATIONAL }
                                                 ; checkWellFormed rest
                                                 }
checkWellFormed (CType OTHER:rest) = do { myTRS <- get 
                                        ; put $ myTRS { trsType = TRSConditional SEMIEQUATIONAL }
                                        ; checkWellFormed rest
                                        }
checkWellFormed (CType JOIN:rest) = do { myTRS <- get 
                                       ; put $ myTRS { trsType = TRSConditional JOIN }
                                       ; checkWellFormed rest
                                       }
checkWellFormed (CType ORIENTED:rest) = do { myTRS <- get 
                                           ; put $ myTRS { trsType = TRSConditional ORIENTED }
                                           ; checkWellFormed rest
                                           }

checkWellFormed (Strategy INNERMOST:rest) = do { myTRS <- get 
                                               ; put $ myTRS { trsStrategy = Just INNERMOST }
                                               ; checkWellFormed rest
                                               }
checkWellFormed (Strategy OUTERMOST:rest) = do { myTRS <- get 
                                               ; put $ myTRS { trsStrategy = Just OUTERMOST }
                                               ; checkWellFormed rest
                                               }
checkWellFormed (Strategy FULL:rest) = do { myTRS <- get 
                                          ; put $ myTRS { trsStrategy = Just FULL }
                                          ; checkWellFormed rest
                                          }
{-
  checkWellFormed (Strategy FULL:rest) = do { myTRS <- get 
                                            ; case trsType myTRS of
                                              TRSEquational -> return . Left $ newErrorMessage (UnExpect $ "found theory in non equational type") (newPos "" 0 0)
                                              _ -> do{put $ myTRS { trsStrategy = Just FULL
                                                                    , trsType = case trsType myTRS of
                                                                                  TRSStandard -> TRSContextSensitive
                                                                                  TRSConditional typ -> TRSContextSensitiveConditional typ
                                                                                  TRSContextSensitive -> TRSContextSensitive
                                                                                  TRSContextSensitiveConditional typ -> TRSContextSensitiveConditional typ
                                                                  }
                                                      ;checkWellFormed rest
                                                    } 
                                            }
-}
checkWellFormed (Strategy (CONTEXTSENSITIVE rmap):rest) = 
     do { myTRS <- get
        ; if (length . nub . map fst $ rmap) == length rmap then
            case trsType myTRS of
              TRSEquational -> return . Left $ newErrorMessage (UnExpect $ "found theory in non equational type") (newPos "" 0 0)
              _ -> do{put $ myTRS { trsRMap = rmap
                                    , trsStrategy = Just FULL
                                    , trsType = case trsType myTRS of
                                                  TRSStandard -> TRSContextSensitive
                                                  TRSConditional typ -> TRSContextSensitiveConditional typ
                                                  TRSContextSensitive -> TRSContextSensitive
                                                  TRSContextSensitiveConditional typ -> TRSContextSensitiveConditional typ
                                  }
                      ;checkWellFormed rest
                      }
          else 
            return . Left $ newErrorMessage (UnExpect $ "duplicated symbols in replacement map declaration") (newPos "" 0 0)
        }

-- checkWellFormed (Context rmap:rest) = addContextSensitive rmap
checkWellFormed (Context rmap:rest) = 
     do { myTRS <- get 
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

checkWellFormed (Theory th:rest) =
    do { myTRS <- get 
          ; case trsType myTRS of
              TRSStandard -> do{put $ myTRS { trsType = TRSEquational } 
                                ;checkWellFormed rest
                                }
              TRSEquational -> checkWellFormed rest
              _ -> return . Left $ newErrorMessage (UnExpect $ "replacementmap or condition type in equational type") (newPos "" 0 0)
        }
{-
  checkWellFormed (Theory th:rest) =
      do { myTRS <- get 
          ;put $ myTRS { trsType = TRSEquational } -- Comprobar si esta en context sensitive?? o conditional???
          ;checkWellFormed rest
          }
-}

checkWellFormed (Signature sg:rest) = do { myTRS <- get 
                                         ; put $ myTRS { signatureBlock = True}
                                         ; result <- checkSignatures sg
                                         ; case result of
                                            Left parseError -> return . Left $ parseError
                                            Right _ -> checkWellFormed rest
                                         }

checkWellFormed (Comment _:rest) = checkWellFormed rest
checkWellFormed ((AnyList _ _):rest ) = checkWellFormed rest


-- | Checks if the signature agree wrt the extracted rules signature
checkSignatures :: [Signdecl] -> State TRS (Either ParseError ())
checkSignatures [] = do { myTRS <- get
                        ; return . Right $ ()
                        {-
                            -- first, we extract the arity of symbols, then we check RMap 
                        ; case trsType myTRS of
                            TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                            TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                            _ -> return . Right $ ()
                        -}
                        }
checkSignatures (s:ss) = do { result <- checkSignature s 
                            ; case result of
                                Left parseError -> return . Left $ parseError 
                                Right _ -> checkSignatures ss
                            }

checkSignature (S id arity) = 
     do { myTRS <- get
        ; let vars = trsVariables myTRS
        ; let funcs = trsSignature myTRS
        ; case (S.member id vars, M.lookup id funcs) of 
            (False, Nothing) -> do { put $ myTRS { trsSignature = M.insert id arity $ funcs }
                                    ; return . Right $ ()
                                    }
            (False, Just len) -> return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arity) ++ " already in signature ") (newPos "" 0 0)
            _ -> return . Left $ newErrorMessage (UnExpect $ "symbol declaration in variables " ++ id) (newPos "" 0 0)
        }
{-
  checkSignature (S id arity) = do { myTRS <- get
                                          ; let funcs = trsSignature myTRS
                                          --; case (M.lookup id funcs) of 
                                              (Just len) -> if (arity == len) then 
                                                      return . Right $ ()
                                                    else
                                                      return . Left $ newErrorMessage (UnExpect $ "arity in signature does not match " ++ id) (newPos "" 0 0)
                                              -- next case is not possible
                                              _ -> return . Left $ newErrorMessage (UnExpect $ "signature of function not in rules " ++ id) (newPos "" 0 0)
                                          }
-}   
checkSignature (Sth id arity thId) =
    if (thId == "A") || (thId == "C") || (thId == "AC") then
      do { myTRS <- get
          ; let vars = trsVariables myTRS
          ; let funcs = trsSignature myTRS
          ; case (S.member id vars, M.lookup id funcs) of 
              (False, Nothing) -> 
                    do { case trsType myTRS of
                            TRSStandard -> do { put $ myTRS {trsSignature = M.insert id arity $ funcs
                                                            , trsType = TRSEquational
                                                            }
                                              ; return . Right $ ()
                                              }
                            TRSEquational -> do { put $ myTRS {trsSignature = M.insert id arity $ funcs}
                                                ; return . Right $ ()
                                                }
                            _ -> return . Left $ newErrorMessage (UnExpect $ "found theory in non equational type") (newPos "" 0 0)
                                      }
              (False, Just len) -> return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arity) ++ " already in signature ") (newPos "" 0 0)
              _ -> return . Left $ newErrorMessage (UnExpect $ "symbols declaration in variables " ++ thId) (newPos "" 0 0)
          }
    else
      return . Left $ newErrorMessage (UnExpect $ "identifier '" ++ thId ++ "' is not valid theory declaration") (newPos "" 0 0)
{-                                                                                                                
  checkSignature (Sth id arity _) = do { myTRS <- get
                                          ; let funcs = trsSignature myTRS
                                          ; case (M.lookup id funcs) of 
                                              (Just len) -> if (arity == len) then 
                                                        --return . Right $ ()
                                                        do { myTRS <- get 
                                                          ; case trsType myTRS of
                                                                TRSStandard -> do { put $ myTRS {trsType = TRSEquational}
                                                                                  ; return . Right $ ()
                                                                                  }
                                                                TRSEquational -> return . Right $ ()
                                                                _ -> return . Left $ newErrorMessage (UnExpect $ "found theory in non equational type") (newPos "" 0 0)
                                                          }
                                                          
                                                    else
                                                        return . Left $ newErrorMessage (UnExpect $ "arity in signature does not match " ++ id) (newPos "" 0 0)
                                              -- next case is not possible
                                              _ -> return . Left $ newErrorMessage (UnExpect $ "signature of function not in rules " ++ id) (newPos "" 0 0)
                                          }
-}
checkSignature (Srp id arity intlist) = 
     do { myTRS <- get
        ; let vars = trsVariables myTRS
        ; let funcs = trsSignature myTRS
        ; let rmap = ((id, intlist):(trsRMap myTRS))
        ; case trsType myTRS of
            TRSEquational -> return . Left $ newErrorMessage (UnExpect $ "found theory in non equational type") (newPos "" 0 0)
            _ -> do{put $ myTRS { trsRMap = rmap
                                  , trsType = case trsType myTRS of
                                                TRSStandard -> TRSContextSensitive
                                                TRSConditional typ -> TRSContextSensitiveConditional typ
                                                TRSContextSensitive -> TRSContextSensitive
                                                TRSContextSensitiveConditional typ -> TRSContextSensitiveConditional typ
                                }
                    ; case (S.member id vars, M.lookup id funcs) of 
                        (False, Nothing) -> do { put $ myTRS { trsSignature = M.insert id arity $ funcs }
                                                ; return . Right $ ()
                                                }
                        (False, Just len) -> return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arity) ++ " already in signature ") (newPos "" 0 0)
                        _ -> return . Left $ newErrorMessage (UnExpect $ "symbols declaration in variables " ++ id) (newPos "" 0 0)
                    }
        }
{-
  checkSignature (Srp id arity intlist) = do { myTRS <- get
                                            ; let funcs = trsSignature myTRS
                                            ; let rmap = ((id, intlist):(trsRMap myTRS))
                                            ; case trsType myTRS of
                                                  TRSEquational -> return . Left $ newErrorMessage (UnExpect $ "found theory in non equational type") (newPos "" 0 0)
                                                  _ -> do{put $ myTRS { trsRMap = rmap
                                                                      , trsType = case trsType myTRS of
                                                                                      TRSStandard -> TRSContextSensitive
                                                                                      TRSConditional typ -> TRSContextSensitiveConditional typ
                                                                                      TRSContextSensitive -> TRSContextSensitive
                                                                                      TRSContextSensitiveConditional typ -> TRSContextSensitiveConditional typ
                                                                      }
                                                          ; case (M.lookup id funcs) of 
                                                              (Just len) -> if (arity == len) then
                                                                              return . Right $ ()
                                                                            else
                                                                              return . Left $ newErrorMessage (UnExpect $ "arity in signature does not match " ++ id) (newPos "" 0 0)
                                                              -- next case is not possible
                                                              _ -> return . Left $ newErrorMessage (UnExpect $ "signature of function not in rules " ++ id) (newPos "" 0 0)
                                                          }
                                            }
-}

-- | Checks if the rules are well-formed wrt the extracted signature
checkRules :: [Rule] -> State TRS (Either ParseError ())
checkRules [] = do { myTRS <- get
                      -- first, we extract the arity of symbols, then we check RMap 
                   ;case trsType myTRS of
                        TRSContextSensitive -> checkRMap . trsRMap $ myTRS
                        TRSContextSensitiveConditional _ -> checkRMap . trsRMap $ myTRS
                        _ -> return . Right $ ()
                   }
checkRules (r:rs) = do { myTRS <- get
                        ; let vs = trsVariables myTRS
                        ; if nonVarLHS vs r then -- lhs (left-hand side of the rule) is non-variable 
                            if isCRule r || (not . hasExtraVars vs $ r) then -- extra variables not allowed in non-conditional rules
                                do { result <- checkTerms . getTerms $ r 
                                   ; case result of
                                        Left parseError -> return . Left $ parseError 
                                        Right _ -> checkRules rs
                                   }
                            else
                              return . Left $ newErrorMessage (UnExpect $ "extra variables in the rule " ++ (show r)) (newPos "" 0 0)
                          else
                            return . Left $ newErrorMessage (UnExpect $ "variable in the left-hand side of the rule " ++ (show r)) (newPos "" 0 0)
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
checkTerm (T id terms) = 
     do { myTRS <- get
        ; let vars = trsVariables myTRS
        ; let funcs = trsSignature myTRS
        ; let signature = signatureBlock myTRS
        ; let arglen = length terms
        ; case (S.member id vars, M.lookup id funcs) of 
            (False, Nothing) -> do {if (signature) then
                                      return . Left $ newErrorMessage (UnExpect $ "symbol: '" ++ id ++ "' not declared in signature ") (newPos "" 0 0)
                                    else
                                      do{ put $ myTRS { trsSignature = M.insert id (length terms) $ funcs }
                                        ; checkTerms terms
                                        }
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
checkTerm (XTerm (Tfun id terms)) = 
     do { myTRS <- get
        ; let funcs = trsSignature myTRS
        ; let signature = signatureBlock myTRS
        ; let arglen = length terms
        ; case (M.lookup id funcs) of
            Nothing -> do {if (signature) then
                            return . Left $ newErrorMessage (UnExpect $ "symbol: '" ++ id ++ "' not declared in signature ") (newPos "" 0 0)
                            else
                            do { put $ myTRS { trsSignature = M.insert id (length terms) $ funcs }
                                ; checkTerms terms
                                }
                          }
            (Just len) -> if (arglen == len) then 
                            checkTerms terms 
                          else
                            return . Left $ newErrorMessage (UnExpect $ "symbol " ++ id ++ " with arity " ++ (show arglen) ++ " in term " ++ (show $ T id terms)) (newPos "" 0 0)
        }
checkTerm (XTerm (Tvar id)) = do { myTRS <- get
                                 ; let vars = trsVariables myTRS
                                 ; case (S.member id vars) of
                                    False -> do { put $ myTRS { trsVariables = S.insert id $ vars }
                                                ; return . Right $ ()
                                                }
                                    _ -> return . Right $ () -- return . Left $ newErrorMessage (UnExpect $ "variable already declared " ++ id) (newPos "" 0 0)
                                  }

-- | Checks if the replacement map satisfies arity restriction and increasing order
checkRMap :: [(Id, [Int])] -> State TRS (Either ParseError ())
checkRMap [] = return . Right $ ()
checkRMap ((f,[]):rmaps) = do { myTRS <- get 
                              ; case M.lookup f (trsSignature myTRS) of 
                                  Just arity -> checkRMap rmaps 
                                  Nothing -> checkRMap rmaps  --return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in rules)") (newPos "" 0 0)
                              }
checkRMap ((f,rmap):rmaps) = do { myTRS <- get 
                                ; case M.lookup f (trsSignature myTRS) of
                                    (Just arity) -> let srmap = sort rmap in
                                                  if (rmap == srmap) && (head rmap >= 1) && (last rmap <= arity) then
                                                    checkRMap rmaps 
                                                  else
                                                    return . Left $ newErrorMessage (UnExpect $ "replacement map for symbol " ++ f ++ " (must be empty" ++ (if arity > 0 then " or an ordered list of numbers in [1.." ++ (show arity) ++ "] " else "") ++ ")") (newPos "" 0 0) 
                                                  --return . Left $ newErrorMessage (UnExpect $ "replacement map for symbol " ++ f ++ " (must be empty" ++ (if arity > 0 then " or an ordered list of numbers in [1.." ++ (show arity) ++ "] separated by commas" else "") ++ ")") (newPos "" 0 0) 
                                    --Nothing -> return . Left $ newErrorMessage (UnExpect $ "function symbol " ++ f ++ " in replacement map (the symbol does not appear in rules)") (newPos "" 0 0)
                                    Nothing -> checkRMap rmaps              
                                }