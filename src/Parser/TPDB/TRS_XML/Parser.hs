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

import Parser.TPDB.TRS.Grammar
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
trsXmlParser = liftM Spec (many (whiteSpace >> (reservedLb "problem" decl) ))
--trsXmlParser = liftM Spec (many1 (whiteSpace >> parens decl))
reservedLb q p= between (aux1 q) (aux2 q) p

aux1 q=do{(symbol "<")
       ;(reserved q)
       ;(symbol ">")
       }

aux2 q=do{(symbol "<")
       ;(symbol "/")
       ;(reserved q)
       ;(symbol ">")
       }

-- | A declaration is form by a set of variables, a theory, a set of
-- rules, a strategy an extra information
decl :: Parser Decl
decl =  (reservedLb "trs" declRules)  -- <|> (reservedLb "theory" declTheory) <|> (reservedLb "" declStrategy) <|> declAnylist <|> declVar

-- | Rules declaration is formed by a reserved word plus a set of
--   rules
declRules :: Parser Decl
declRules = liftM Rules (many (reservedLb "rules" rule))

-- | Rule
rule :: Parser Rule
rule =
 do sr <- reservedLb "rule" simpleRule
    conds <- reservedLb "conditions" ( option [] (many $ reservedLb "condition" cond))
    return (Rule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- reservedLb "lhs" term
    op <- ruleOps
    t2 <- reservedLb "rhs" term
    return (op t1 t2)

-- | Rule options
ruleOps = try ({-reservedOp "->" >>-} return (:->){-(Flecha)-}) --do{ try (reservedOp "->"; return (:->) }
        -- <|> (reservedOp "->=" >> return (:->=){-(FlechaIgual)-})

-- | Condition
cond =
 do -- option 1 (brackets natural)
    t1 <- reservedLb "lhs" term
    op <- condOps
    t2 <- reservedLb "rhs" term
    return (op t1 t2)

-- | Condition options
condOps = try {-(reservedOp "-><-" >> return (:-><-))
        <|>-} ({-reservedOp "->" >>-} return (Arrow)) --do{ try (reservedOp "->"; return (:->) }
--        <|> (reservedOp "-><-" >> return (:-><-)) 

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
    terms <- option [] (many1 (reservedLb "arg" term)) -- terms <- parens (many (commaSep' term) ) 
    return (Tfun n terms)

{-
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
    do sthd <- parens $ simpleThdecl
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
-}

{-
declStrategy :: Parser Decl
declStrategy = reserved "STRATEGY" >> liftM Strategy (innermost <|> outermost <|> contextsensitive)

-- | innermost strategy
innermost :: Parser Strategydecl
innermost = reserved "INNERMOST" >> return INNERMOST

-- | outermost strategy
outermost :: Parser Strategydecl
outermost = reserved "OUTERMOST" >> return OUTERMOST

-- | contextsensitive strategy
contextsensitive :: Parser Strategydecl
contextsensitive =
 do reserved "CONTEXTSENSITIVE"
    strats <- many$ parens (do a <- identifier
                               b <- many natural
                               return $ Csstrat (a, map fromInteger b) -- !!
                           )
    return $ CONTEXTSENSITIVE strats
-}
{- 
-- | contextsensitive strategy - Option 2
contextsensitive :: Parser Strategydecl
contextsensitive = reserved "CONTEXTSENSITIVE" >> liftM CONTEXTSENSITIVE (many csstrat)
csstrat :: Parser Csstrat
csstrat =
 do strats <- parens (do a <- identifier
                         b <- many natural
                         return (a, map fromInteger b) -- !!
                     )
    return$ Csstrat strats
-}

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



{-
do{ char '?'
               ; reservedLb
               ; char ')'
               ; reservedLb
               }
<|> return ()
-}
{-
-- | XML labels
reservedLb :: CharParser st a -> CharParser st a
reservedLb r= between (string "trs") (string "trs") lexer

--reservedLbAux p= between (symbol "<") (symbol ">") p
-}