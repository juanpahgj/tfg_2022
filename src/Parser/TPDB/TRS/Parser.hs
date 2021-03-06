
module Parser.TPDB.TRS.Parser (

-- * Exported functions

trsParser

) where

import Parser.Grammar
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
trsParser = liftM Spec (many1 (whiteSpace >> parens decl))

-- | A declaration is form by a set of variables, a theory, a set of rules, a strategy an extra information
decl :: Parser Decl
decl = declVar <|> declRules <|> declStrategy <|> declTheory <|> declAnylist

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
    return (Rule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- term
    op <- ruleOps
    t2 <- term
    return (op t1 t2)

-- | Rule options
ruleOps = try (reservedOp "->" >> return (:->))
        <|> (reservedOp "->=" >> return (:->=))

-- | Condition
cond =
 do
    t1 <- term
    op <- condOps
    t2 <- term
    return (op t1 t2)

-- | Condition options
condOps = try (reservedOp "-><-" >> return (:-><-))
        <|> (reservedOp "->" >> return (Arrow))

-- | A term
term :: Parser Term
term =
 do n <- identifier
    terms <- option [] (parens (commaSep' term))
    return (T n terms)


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
    do  reserved "CONTEXTSENSITIVE"
        strats <- many$ parens (do a <- identifier
                                   b <- many natural
                                   return (a, map fromInteger b)
                                )
        return $ CONTEXTSENSITIVE strats


-- | Theory declaration is formed by a reserved word plus a set of theory declarations
declTheory :: Parser Decl
declTheory = reserved "THEORY" >> liftM Theory (many thdecl)

-- | Theory declaration
thdecl :: Parser Thdecl
thdecl =
 do sthd <- parens $ simpleThdecl
    listofthdecl <- option [] (many thdecl)
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
