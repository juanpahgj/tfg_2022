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
module Parser.TPDB.TRS_XML.Parser (

-- * Exported functions

trsXmlParser, term

) where

import Parser.TPDB.Grammar -- import Parser.TPDB.TRS.Grammar
import Parser.TPDB.TRS_XML.Scanner

import Text.ParserCombinators.Parsec (Parser(..), many, (<|>), many1, sepEndBy, between
  , option, char, sepBy, try, noneOf, digit)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- |parse TRS specification
trsXmlParser :: Parser Spec
trsXmlParser = whiteSpace >> (reservedLb "problem" $ liftM Pre (many preDecl)) 
    --whiteSpace >> (reservedLb "problem" (many $ trs <|> strategy))

--liftM Spec (whiteSpace >> (reservedLb "trs" $ many (whiteSpace >> decl)) )

preDecl =  trs <|> strategy

strategy :: Parser Predecl
strategy = reservedLb "strategy" $ liftM Strgy (innermost <|> outermost <|> contextsensitive)

trs :: Parser Predecl
trs = reservedLb "trs" $ liftM Decs (many decl)

-- | A declaration is form by a set of variables, a theory, a set of
-- rules, a strategy an extra information
decl :: Parser Decl
decl = declRules <|> ctypeDecl <|> declSignature -- <|> declAnylist <|> declVar

-- | Rules declaration is formed by a reserved word plus a set of
--   rules
declRules :: Parser Decl
declRules = reservedLb "rules" $ liftM Rules (many $ reservedLb "rule" rule)

-- | Rule
rule :: Parser Rule
rule =
 do sr <- simpleRule
    conds <- option [] (reservedLb "conditions" (many $ reservedLb "condition" cond))
    return (Rule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- reservedLb "lhs" term
    op <- ruleOps
    t2 <- reservedLb "rhs" term
    return (op t1 t2)

-- | Rule options
ruleOps = try (return (:->)) 

-- | Condition
cond =
 do -- option 1 (brackets natural)
    t1 <- reservedLb "lhs" term
    op <- condOps
    t2 <- reservedLb "rhs" term
    return (op t1 t2)

-- | Condition options
condOps = try (return (Arrow))
-- | A term
term :: Parser Term
term =                                         -- !!!!!!!ests mal-incompleto
 do xmlTerm <- (try termVar) <|> (reservedLb "funapp" termFun)
    return (XTerm xmlTerm)

termVar :: Parser XmlTerm
termVar = liftM Tvar (reservedLb "var" identifier)

termFun :: Parser XmlTerm
termFun =                                              -- !!!!!!!ests mal-incompleto
 do n <- reservedLb "name" identifier
    terms <- (many1 (try $ reservedLb "arg" term)) -- terms <- parens (many (commaSep' term) ) 
    return (Tfun n terms)




--strategy :: Parser Spec
--strategy = reservedLb "strategy" $ liftM Strtgy (innermost <|> outermost <|> contextsensitive)

-- | innermost strategy
innermost :: Parser Strategydecl
innermost = reserved "INNERMOST" >> return INNERMOST

-- | outermost strategy
outermost :: Parser Strategydecl
outermost = reserved "OUTERMOST" >> return OUTERMOST

-- | contextsensitive strategy
contextsensitive :: Parser Strategydecl
contextsensitive = reserved "FULL" >> return FULL
 {-do reserved "CONTEXTSENSITIVE"
    strats <- many$ parens (do a <- identifier
                               b <- many natural
                               return $ Csstrat (a, map fromInteger b) -- !!
                           )
    return $ CONTEXTSENSITIVE strats
 -}

ctypeDecl :: Parser Decl
ctypeDecl = reservedLb "conditiontype" $ liftM CType (join <|> oriented <|> other)

-- | innermost strategy
join :: Parser CondType
join = reserved "JOIN" >> return JOIN

-- | outermost strategy
oriented :: Parser CondType
oriented = reserved "ORIENTED" >> return ORIENTED

-- | contextsensitive strategy
other :: Parser CondType
other = reserved "OTHER" >> return OTHER

-- | Signature declaration is formed by list of functions with arity
declSignature :: Parser Decl
declSignature = reservedLb "signature" $ liftM Signature (many (reservedLb "funcsym" fun)) --reserved "SIG" >> liftM Signature (many (parens fun))

-- | Function symbol
fun :: Parser Signdecl -- (Id,Int)
fun = try (do{ n <- reservedLb "name" identifier
             ; m <- reservedLb "arity" natural -- (many1 digit)
             ; th <- reservedLb "theory" thsig
             ;  return (Sth n (fromInteger m) th) -- return (Sth n (read m) th)
             })
    <|> try (do{ n <- reservedLb "name" identifier
               ; m <- reservedLb "arity" natural -- m <- reservedLb "arity" (many1 digit)
               ; rp <- reservedLb "replacementmap" (many $ reservedLb "entry" natural)
               ; return (Srp n (fromInteger m) (map fromInteger rp)) -- return (Srp n (read m) (rp))
               })
    <|> do{ n <- reservedLb "name" identifier
          ; m <- reservedLb "arity" natural -- m <- reservedLb "arity" (many1 digit)
          ; return (S n (fromInteger m)) -- return (S n (read m))
          }

thsig = (thSigA <|> thSigC <|> thSigAC)

thSigA :: Parser Signthry 
thSigA = reserved "A" >> return A

thSigC :: Parser Signthry 
thSigC = reserved "C" >> return C

thSigAC :: Parser Signthry 
thSigAC = reserved "AC" >> return AC

{-
    -- | Theory identificator list
    thlId =
    do id <- identifier
        idlist <- option [] phrase 
        return (Id id idlist)

-}

{-
    -- | Variables declaration is formed by a reserved word plus a set of
    --   variables
    declVar :: Parser Decl
    declVar = reserved "VAR" >> do { idList <- phrase
                                ; return . Var $ idList
                                }
-}

{-
    declAnylist :: Parser Decl
    declAnylist=
    do 
        name <- identifier
        decls <- many anyContent
        return $ AnyList name decls

    anyContent :: Parser AnyContent
    anyContent = anyId <|> anySt <|> anyAC <|> (comma >> anyContent)
        
    -- | Identifiers
    anyId :: Parser AnyContent
    anyId = liftM AnyId identifier

    -- | Strings
    anySt :: Parser AnyContent
    anySt = liftM AnySt stringLiteral

    -- | Others
    anyAC :: Parser AnyContent
    anyAC = liftM AnyAC (parens $ many anyContent)
-}


-- | Extra information

-- | XML labels
--reservedLb :: CharParser st a -> CharParser st a
reservedLb q p=between (try $ aux1 q) (try $ aux2 q) p -- (try(aux1 q)) (aux2 q) p

aux1 q=do{(symbol "<")
;(reserved q)
;(symbol ">")
}

aux2 q=do{(symbol "<")
;(symbol "/")
;(reserved q)
;(symbol ">")
}

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

