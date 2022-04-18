-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.TPDB.TRS.Parser
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the parser for TRSs in TPDB format
--
-----------------------------------------------------------------------------
module Parser.TPDB.TRS.Parser (

-- * Exported functions

trsParser, term

) where

import Parser.TPDB.TRS.Grammar
import Parser.TPDB.TRS.Scanner

import Text.ParserCombinators.Parsec (Parser(..), many, (<|>), many1, sepEndBy
  , option, char, sepBy, try, noneOf, digit)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- |parse TRS specification
trsParser :: Parser Spec
trsParser = liftM Spec (many (whiteSpace >> parens decl))
--trsParser = liftM Spec (many1 (whiteSpace >> parens decl))

-- | A declaration is form by a set of variables, a theory, a set of
-- rules, a strategy an extra information
decl :: Parser Decl
decl = declVar <|> declTheory <|> declRules 

-- | Variables declaration is formed by a reserved word plus a set of
--   variables
declVar :: Parser Decl
declVar = reserved "VAR" >> do { idList <- phrase
                               ; return . Var $ idList
                               }

-- | Theory declaration is formed by a reserved word plus a set of
--   theory declarations
declTheory :: Parser Decl
declTheory = reserved "THEORY" >> liftM Theory (many thdecl)

-- | Theory declaration
thdecl :: Parser Thdecl
thdecl =
 do 
    sthd <- parens $ simpleThdecl
    listofthdecl <- option [] (many thdecl) --listofthdecl <- many thdecl
    return (Thdecl sthd listofthdecl)

-- | Simple theory declaration
simpleThdecl :: Parser SimpleThdecl
simpleThdecl = thlId <|> declEq

-- | Theory identificator list
thlId =
 do id <- identifier
    idlist <- option [] phrase 
    return (Id id idlist)

-- | Equation declaration
declEq = reserved "EQUATIONS" >> liftM Equations (many eq)

-- | Equation
eq =
 do t1 <- term
    op <- eqOps
    t2 <- term
    return (op t1 t2)

-- | Equation options
eqOps = (reservedOp "==" >> return (:==:))

-- | A term
term :: Parser Term
term =
 do n <- identifier
    terms <- option [] (parens (commaSep' term))
    return (T n terms)

-- | Rules declaration is formed by a reserved word plus a set of
--   rules
declRules :: Parser Decl
declRules = reserved "RULES" >> liftM Rules (many rule)

-- | Rule
rule :: Parser Rule
rule =
 do sr <- simpleRule
    conds <- option [] (reservedOp "|" >> commaSep' cond)
    return (Rule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- term
    op <- ruleOps
    t2 <- term
    return (op t1 t2)

-- | Rule options
ruleOps = try (reservedOp "->" >> return (:->){-(Flecha)-}) --do{ try (reservedOp "->"; return (:->) }
        <|> (reservedOp "->=" >> return (:->=){-(FlechaIgual)-})

-- | Condition
cond =
 do -- option 1 (brackets natural)
    t1 <- term
    op <- condOps
    t2 <- term
    return (op t1 t2)

-- | Condition options
condOps = (reservedOp "-><-" >> return (:-><-))
--condOps = try (reservedOp "->" >> return (:->)) --do{ try (reservedOp "->"; return (:->) }
--        <|> (reservedOp "-><-" >> return (:-><-)) 

{-

-- | Oriented conditions
oriented :: Parser CondType
oriented = reserved "ORIENTED" >> return Oriented

-}


{-



-- | Context-sensitive strategy
declCSStrategy :: Parser Decl
declCSStrategy =
 do reserved "REPLACEMENT-MAP"
    strats <- many$ parens (do a <- identifier
                               b <- commaSep' natural
                               return (a, map fromInteger b)
                           )
    return $ Context strats

-}


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

{-- | Signature declaration is formed by list of functions with arity
declSignature :: Parser Decl
declSignature = reserved "SIG" >> liftM Signature (many (parens fun))

-- | Function symbol
fun :: Parser (Id,Int)
fun =
 do n <- identifier
    m <- many1 digit
    return (n,read m)
--}