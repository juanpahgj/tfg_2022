{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Grammar (

-- * Exported data

Spec (..), Decl (..), Thdecl (..), SimpleThdecl (..), Equation (..)
, Term (..), XmlTerm (..), Rule(..), SimpleRule (..), Cond (..), Strategydecl (..)
, AnyContent (..), Id, TRSType (..), TRS (..), CondType (..), Signdecl (..)

-- * Exported functions

, getTerms, nonVarLHS, isCRule, hasExtraVars

) where

import Data.Typeable
import Data.Generics
import Data.Map (Map)
import Data.Set as S (Set, member, unions, insert, (\\), null, empty)
import Data.List (intersperse)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Specification declaration
data Spec = Spec [Decl] -- ^ List of declarations
      deriving (Eq, Ord, Show, Data, Typeable)

-- | List of declarations  (The order of the constructors in this data type is important for later semantic checking in the parser module !!)
data Decl = Var [Id] -- ^ Set of variables
    | CType CondType -- ^ Type of conditional rules (XML format)
    | Strategy Strategydecl -- ^ Extra information
    | Context [(Id, [Int])] -- ^ Context-Sensitive strategy (COPS format)
    | Theory [Thdecl] -- ^ Set of rules
    | Signature [Signdecl] -- ^ Type of signature (XML format)
    | Rules [Rule] -- ^ Set of rules
    | AnyList Id [AnyContent] --AnyList Id [String]
    | Comment String -- ^ Extra information
      deriving (Eq, Ord, Show, Data, Typeable)

-- | Theory declaration
data Thdecl = Thdecl SimpleThdecl [Thdecl]
      deriving (Eq, Ord, Data, Typeable)

-- | Simple theory declaration
data SimpleThdecl = Id Id [Id]
    | Equations [Equation] 
      deriving (Eq, Ord, Data, Typeable)

-- | Equation declaration
data Equation = Term :==: Term -- ^ Equation
      deriving (Eq, Ord, Data, Typeable)

-- | Term declaration
data Term = T Id [Term] -- ^ Term
    | XTerm XmlTerm
    deriving (Eq, Ord, Data, Typeable)

-- | XmlTerm declaration (for xml)
data XmlTerm = Tfun Id [Term] -- ^ Term
    | Tvar Id -- Added for XML format
    deriving (Eq, Ord, Data, Typeable)

-- | Rule declaration
data Rule = Rule SimpleRule [Cond] -- ^ Conditional rewriting rule
      | COPSrule SimpleRule [Equation] -- ^ COPS format
      deriving (Eq, Ord, Data, Typeable)

-- | Simple rule declaration
data SimpleRule = Term :-> Term
    | Term :->= Term
    deriving (Eq, Ord, Data, Typeable)

data Cond = Term :-><- Term --
    | Arrow Term Term -- | Term :-> Term
    deriving (Eq, Ord, Data, Typeable)

-- | Strategy Declaration
data Strategydecl = INNERMOST
  | OUTERMOST
  | CONTEXTSENSITIVE [(Id, [Int])]
  | FULL -- Added for XML format. Equivalent to CONTEXTSENSITIVE
    deriving (Eq, Ord, Show, Data, Typeable)

data AnyContent = AnyId Id
  | AnySt String
  | AnyAC [AnyContent]
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Condition Type
data CondType = JOIN
  | ORIENTED
  | OTHER  -- Added for XML format. Equivalent to SEMIEQUATIONAL
  | SEMIEQUATIONAL -- COPS
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Signature declaration
data Signdecl = S Id Int
  | Sth Id Int Id
  | Srp Id Int [Int] --replacementmap  
    deriving (Eq, Ord, Data, Typeable)

-- | Identifier
type Id = String

-- | TSR Type
data TRSType = 
  TRSStandard
  | TRSEquational
  | TRSConditional CondType
  | TRSContextSensitive 
  | TRSContextSensitiveConditional CondType
    deriving (Show)

data TRS 
  = TRS { trsSignature :: Map Id Int
        , trsVariables :: Set Id
        , trsRMap :: [(Id, [Int])]
        , trsRules :: [Rule]
        , trsType :: TRSType
        , trsStrategy :: Maybe Strategydecl
        , signatureBlock :: Bool
        } deriving (Show)

-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------

-- Show

instance Show SimpleThdecl where 
  show (Id id []) = show id
  show (Id id ids) = show id ++ " | " ++ (concat . intersperse ", " . map show $ ids)
  -- show (Equations eqs) = (concat . intersperse ", " . map show $ eqs)

instance Show Thdecl where 
  show (Thdecl t []) = show t
  show (Thdecl t ths) = show t ++ " | " ++ (concat . intersperse ", " . map show $ ths)

instance Show Equation where -- instance Show Equation where
  show (t1 :==: t2) = show t1 ++ " == " ++ show t2

instance Show XmlTerm where
  show (Tfun f []) = f 
  show (Tfun f terms) = f ++ "(" ++ (concat . intersperse "," . map show $ terms) ++ ")" 
  show (Tvar v) = v

instance Show Term where
  show (T f []) = f 
  show (T f terms) = f ++ "(" ++ (concat . intersperse "," . map show $ terms) ++ ")" 
  show (XTerm xterm) = show xterm

instance Show SimpleRule where 
  show (t1 :-> t2) = show t1 ++ " -> " ++ show t2
  show (t1 :->= t2) = show t1 ++ " ->= " ++ show t2
  
instance Show Rule where
  show (Rule r []) = show r
  show (COPSrule r []) = show r
  show (Rule r conds) = show r ++ " | " ++ (concat . intersperse ", " . map show $ conds)
  show (COPSrule r eqs) = show r ++ " | " ++ (concat . intersperse ", " . map show $ eqs)

instance Show Cond where
  show (t1 :-><- t2) = show t1 ++ " -><- " ++ show t2
  show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2

instance Show Signdecl where 
  show (S t i) = show t ++ " arity: " ++ show i
  show (Sth t i th) = show t ++ " arity: " ++ show i ++ " theory: " ++ show th
  show (Srp t i rps) = show t ++ " arity: " ++ show i ++ " rpmap: " ++ (concat . intersperse " " . map show $ rps)


-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | gets all the terms from a rule
getVars :: Set Id -> Term -> Set Id
getVars vs (T idt ts) = getVarsAux vs idt ts
getVars vs (XTerm (Tfun idt ts)) = getVarsAux vs idt ts
getVars vs (XTerm (Tvar idt)) = let tsVars = S.empty
                                      in if member idt vs then
                                          insert idt tsVars 
                                        else 
                                          tsVars

getVarsAux vs idt ts= let tsVars = unions . map (getVars vs) $ ts
                        in if member idt vs then
                             insert idt tsVars 
                           else 
                             tsVars

-- | gets all the terms from a rule
getTerms :: Rule -> [Term]
getTerms (Rule (l :-> r) conds) = (l:r:concatMap getTermsCond conds)
getTerms (Rule (l :->= r) conds) = (l:r:concatMap getTermsCond conds)
getTerms (COPSrule (l :-> r) eqs) = (l:r:concatMap getTermsEq eqs)

-- | gets all the terms from a equation
getTermsEq :: Equation -> [Term]
getTermsEq (l :==: r) = [l,r]

-- | gets all the terms from a equation
getTermsCond :: Cond -> [Term]
getTermsCond (l :-><- r) = [l,r]
getTermsCond (Arrow l r) = [l,r]

-- | checks if the lhs is non-variable
nonVarLHS :: Set Id -> Rule -> Bool
nonVarLHS vs (Rule ((T idt _) :-> r) conds) = not . member idt $ vs
nonVarLHS vs (Rule ((T idt _) :->= r) conds) = not . member idt $ vs
nonVarLHS vs (Rule ((XTerm (Tfun idt _)) :-> r) conds) = not . member idt $ vs
nonVarLHS vs (Rule ((XTerm (Tvar idt)) :-> r) conds) = not . member idt $ vs
nonVarLHS vs (COPSrule ((T idt _) :-> r) eqs) = not . member idt $ vs 

-- | checks if the rule is conditional
isCRule :: Rule -> Bool
isCRule (Rule _ []) = False
isCRule (COPSrule _ []) = False
isCRule _ = True

-- | checks if the non-conditional rule has extra variables
hasExtraVars :: Set Id -> Rule -> Bool
hasExtraVars vs (Rule (l :-> r) []) = not . S.null $ getVars vs r \\ getVars vs l
hasExtraVars vs (Rule (l :->= r) []) = not . S.null $ getVars vs r \\ getVars vs l
hasExtraVars vs (COPSrule (l :-> r) []) = not . S.null $ getVars vs r \\ getVars vs l
hasExtraVars _ _ = error $ "Error: hasExtraVars only applies to non-conditional rules"
