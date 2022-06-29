-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.COPS.TRS.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in COPS format
--
-----------------------------------------------------------------------------
module Parser.COPS.TRS.Parser (

-- * Exported functions

trsCOPSParser

) where

import Parser.Grammar
import Parser.COPS.TRS.Scanner

import Text.ParserCombinators.Parsec (Parser(..), many, (<|>), many1, sepEndBy
  , option, char, sepBy, try, noneOf, digit)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- |parse TRS specification
trsCOPSParser :: Parser Spec
trsCOPSParser = liftM Spec (many1 (whiteSpace >> parens decl))

-- | A declaration is form by a set of variables, a signature, a set of rules, a strategy, a condition, an extra information
decl :: Parser Decl
decl = declVar <|> declRules <|>  declCSStrategy <|> declCondType <|> declSignature <|> declComment

-- | Variables declaration is formed by a reserved word plus a set of variables
declVar :: Parser Decl
declVar = reserved "VAR" >> do { idList <- phrase
                               ; return . Var $ idList
                               }

-- | Rules declaration is formed by a reserved word plus a set of rules
declRules :: Parser Decl
declRules = reserved "RULES" >> liftM Rules (many rule)

-- | Rule
rule :: Parser Rule
rule =
 do sr <- simpleRule
    conds <- option [] (reservedOp "|" >> commaSep' cond)
    return (COPSrule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- term
    op <- ruleOps
    t2 <- term
    return (op t1 t2)

-- | Rule options
ruleOps = (reservedOp "->" >> return (:->))

-- | Condition
cond =
 do
    t1 <- term
    op <- condOps
    t2 <- term
    return (op t1 t2)

-- | Condition options
condOps = (reservedOp "==" >> return (:==:))

-- | A term
term :: Parser Term
term =
 do n <- identifier
    terms <- option [] (parens (commaSep' term))
    return (T n terms)


-- | Context-sensitive strategy
declCSStrategy :: Parser Decl
declCSStrategy =
 do reserved "REPLACEMENT-MAP"
    strats <- many1$ parens (do a <- identifier
                                b <- commaSep' natural
                                return (a, map fromInteger b)
                            )
    return $ Context strats


-- | Condition type declaration is formed by a reserved word plus SEMI-EQUATIONAL, JOIN, or ORIENTED
declCondType :: Parser Decl
declCondType = reserved "CONDITIONTYPE" >> liftM CType (semiEq <|> join <|> oriented)

-- | Semi-equational conditions
semiEq :: Parser CondType
semiEq = reserved "SEMI-EQUATIONAL" >> return SEMIEQUATIONAL

-- | Join conditions
join :: Parser CondType
join = reserved "JOIN" >> return JOIN

-- | Oriented conditions
oriented :: Parser CondType
oriented = reserved "ORIENTED" >> return ORIENTED


-- | Signature declaration is formed by list of functions with arity
declSignature :: Parser Decl
declSignature = reserved "SIG" >> liftM Signature (many (parens fun))

-- | Function symbol
fun :: Parser Signdecl
fun =
 do n <- identifier
    m <- many1 digit
    return (S n (read m))


declComment =
 do reserved "COMMENT"
    decls <- many $ noneOf ")"
    return$ Comment decls


-- | Extra information
-- | A phrase
phrase = many identifier

-- | Separated by comma
commaSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
commaSep' = (`sepEndBy` comma)

-- | Separated by semicolon
semicolonSep' :: Text.ParserCombinators.Parsec.Prim.GenParser Char () a
             -> Text.ParserCombinators.Parsec.Prim.GenParser Char () [a]
semicolonSep' = (`sepBy` semi)
