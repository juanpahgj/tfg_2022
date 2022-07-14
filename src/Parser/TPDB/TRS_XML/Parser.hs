
module Parser.TPDB.TRS_XML.Parser (

-- * Exported functions

trsXmlParser

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
                             ; return $ Spec (st:d)
                             })


-- | A declaration is form by a set of rules, signatures, a condition type and comments
decl :: Parser Decl
decl = declRules <|> declSignature <|> ctypeDecl <|> declComment

-- | Rules declaration is formed by reserved tags plus a set of rules
declRules :: Parser Decl
declRules = reservedLb "rules" $ liftM Rules (many $ reservedLb "rule" rule) --(rulesType)

--rulesType =(try $ reservedLb "relrules" $ (many (reservedLb "rule" relRule)) )
--        <|> (try $ many (try $ reservedLb "rule" rule)) 


-- | Rule
rule :: Parser Rule
rule =
 do sr <- simpleRule
    conds <- option [] (reservedLb "conditions" (many $ reservedLb "condition" cond))
    return (Rule sr conds)

-- | Simple rule
simpleRule =
 do t1 <- reservedLb "lhs" term
    op <- return (:->)  --ruleOps
    t2 <- reservedLb "rhs" term
    return (op t1 t2)

-- | Rule options
--ruleOps = try (return (:->)) 

{-
    -- | Relative rule
    relRule :: Parser Rule
    relRule =
    do sr <- simpleRelRule
        conds <- option [] (reservedLb "conditions" (many $ reservedLb "condition" cond))
        return (Rule sr conds)

    -- | Simple relative rule
    simpleRelRule =
    do t1 <- reservedLb "lhs" term
        op <- return (:->=)
        t2 <- reservedLb "rhs" term
        return (op t1 t2)
-}

-- | Condition
cond =
 do
    t1 <- reservedLb "lhs" term
    op <- condOps
    t2 <- reservedLb "rhs" term
    return (op t1 t2)

-- | Condition options
condOps = try (return (Arrow))

-- | A term
term :: Parser Term
term =
 do xmlTerm <- (try termVar) <|> (reservedLb "funapp" termFun)
    return (XTerm xmlTerm)

termVar :: Parser XmlTerm
termVar = liftM Tvar (reservedLb "var" identifier)

termFun :: Parser XmlTerm
termFun =
 do n <- reservedLb "name" identifier
    terms <- (many (try $ reservedLb "arg" term))
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
declSignature = reservedLb "signature" $ liftM Signature (many (try $ reservedLb "funcsym" fun))

-- | Function symbol
fun :: Parser Signdecl -- (Id,Int)
fun = (sigTh) <|> (sigRpNull) <|> (sigRp)  <|> (sig)

sigRpNull = try (do{ n <- reservedLb "name" identifier
                   ; m <- reservedLb "arity" natural
                   ; try $ emptyReservedLb "replacementmap"
                   ; return (Srp n (fromInteger m) [])
                   })

sigRp = try (do { n <- reservedLb "name" identifier
                ; m <- reservedLb "arity" natural
                ; rp <- reservedLb "replacementmap" (many1 $ reservedLb "entry" natural)
                ; return (Srp n (fromInteger m) (map fromInteger rp))
                })

sig = try (do{ n <- reservedLb "name" identifier
             ; m <- reservedLb "arity" natural
             ; return (S n (fromInteger m))
             })

sigTh = try (do { n <- reservedLb "name" identifier
                ; m <- reservedLb "arity" natural
                ; th <- reservedLb "theory" identifier
                ; return (Sth n (fromInteger m) th)
                })
                

-- | Extra information
declComment = liftM Comment (reservedLb "comment" (many $ noneOf "<"))


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
                       ; (string q)
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
