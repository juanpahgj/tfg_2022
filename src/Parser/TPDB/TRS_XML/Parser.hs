
module Parser.TPDB.TRS_XML.Parser (

-- * Exported functions

trsXmlParser, term

) where

import Parser.Grammar
import Parser.TPDB.TRS_XML.Scanner

import Text.ParserCombinators.Parsec (Parser(..), many, (<|>), many1, sepEndBy, between
  , option, char, sepBy, try, noneOf, digit, anyChar, skipMany, manyTill, string)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Monad (liftM)
import Data.List (concat, insert)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- |parse TRS specification
trsXmlParser :: Parser Spec
trsXmlParser = whiteSpace >> (many $ try metainf) >> whiteSpace >>
    (reservedLb "problem" $ do{d <- reservedLb "trs" (many decl)
                             ; st <- strategy
                             ; (many $ try metainf)
                             ; whiteSpace
                             ; return $ Spec (st:d) --Spec (insert st d)
                             })

{-
    -- (liftM Spec . insert) (strategy (reservedLb "trs" (many decl)) ) )

    --trsXmlParser = whiteSpace >> (reservedLb "problem" $ 
    --  (reservedLb "trs" (liftM Spec (many decl) )) >> (liftM Spec (many strategy) ))

    --trsXmlParser = whiteSpace >> (reservedLb "problem" $ liftM Spec . concat $ many $ decls)

    --aplanar :: Parser [[Decl]] -> Parser [Decl]
    --aplanar l = concat l

    --decls :: Parser [[Decl]]
    --decls = decl <|> strategy
-}

-- | A declaration is form by a set of variables, a signatures and a condition type
decl :: Parser Decl
decl = declRules <|> declSignature <|> ctypeDecl <|> declComment

-- | Rules declaration is formed by reserved tags plus a set of rules
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
    terms <- (many (try $ reservedLb "arg" term)) -- terms <- parens (many (commaSep' term) ) 
    return (Tfun n terms)


strategy :: Parser Decl
strategy = reservedLb "strategy" $ liftM Strategy (innermost <|> outermost <|> contextsensitive)

-- | innermost strategy
innermost :: Parser Strategydecl
innermost = reserved "INNERMOST" >> return INNERMOST

-- | outermost strategy
outermost :: Parser Strategydecl
outermost = reserved "OUTERMOST" >> return OUTERMOST

-- | full strategy
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

-- | join condition type
join :: Parser CondType
join = reserved "JOIN" >> return JOIN

-- | oriented condition type
oriented :: Parser CondType
oriented = reserved "ORIENTED" >> return ORIENTED

-- | other condition type
other :: Parser CondType
other = reserved "OTHER" >> return OTHER

-- | Signature declaration is formed by list of functions with arity
declSignature :: Parser Decl
declSignature = reservedLb "signature" $ liftM Signature (many (reservedLb "funcsym" fun)) --reserved "SIG" >> liftM Signature (many (parens fun))

-- | Function symbol
fun :: Parser Signdecl -- (Id,Int)
fun = (sigTh) <|> (sigRpNull) <|> (sigRp)  <|> (sig)

sigRpNull = try (do{ n <- reservedLb "name" identifier
                   ; m <- reservedLb "arity" natural -- m <- reservedLb "arity" (many1 digit)
                   ; try $ emptyReservedLb "replacementmap"
                   ; return (Srp n (fromInteger m) [])
                   })

sigRp = try (do { n <- reservedLb "name" identifier
                ; m <- reservedLb "arity" natural -- m <- reservedLb "arity" (many1 digit)
                ; rp <- reservedLb "replacementmap" (many1 $ reservedLb "entry" natural)
                ; return (Srp n (fromInteger m) (map fromInteger rp)) -- return (Srp n (read m) (rp))
                })

sig = try (do{ n <- reservedLb "name" identifier
             ; m <- reservedLb "arity" natural -- m <- reservedLb "arity" (many1 digit)
             ; return (S n (fromInteger m)) -- return (S n (read m))
             })

sigTh = try (do { n <- reservedLb "name" identifier
                ; m <- reservedLb "arity" natural -- (many1 digit)
                ; th <- reservedLb "theory" thsig
                ; return (Sth n (fromInteger m) th) -- return (Sth n (read m) th)
                })

thsig = (thSigA <|> thSigC <|> thSigAC)

thSigA :: Parser Signthry 
thSigA = reserved "A" >> return A

thSigC :: Parser Signthry 
thSigC = reserved "C" >> return C

thSigAC :: Parser Signthry 
thSigAC = reserved "AC" >> return AC


-- | Extra information
declComment = liftM Comment (reservedLb "comment" (many $ noneOf "<"))

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

aux1 q=do{ (symbol "<")
         ; (reserved q)
         ; manyTill anyChar (try (symbol ">")) -- (symbol ">")
         }

aux2 q=do{ (symbol "<")
         ; (symbol "/")
         ; (reserved q)
         ; (symbol ">")
         }

emptyReservedLb q = do { (symbol "<")
                        ; (reserved q)
                        ; (symbol "/")
                        ; (symbol ">")
                        }

metainf = try (do{ whiteSpace
                 ; string "<?"
                 ; manyTill anyChar (try (string "?>"))
                 })
          <|> (do{ whiteSpace
                 ; string "<metainformation>"
                 ; manyTill anyChar (try (string "</metainformation>"))
                 })

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

