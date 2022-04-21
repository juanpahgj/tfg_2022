{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.COPS.TRS.Grammar
-- Copyright   :  (c) muterm development team
-- License     :  see LICENSE
--
-- Maintainer  :  r.gutierrez@upm.es
-- Stability   :  unstable
-- Portability :  non-portable
--
-- This module manage the grammar for TRSs in COPS
--
-----------------------------------------------------------------------------

module Parser.TPDB.TRS.Grammar (

-- * Exported data

Spec(..), Decl(..), Thdecl (..), SimpleThdecl (..), Equation (..) --, SimpleEquation (..)
, Term (..), Rule(..), SimpleRule (..), Cond (..), Strategy (..)
, Csstratlist (..), Id, TRSType (..), TRS (..)


-- * Exported functions

--, getTerms, nonVarLHS, isCRule, hasExtraVars

) where

import Data.Typeable
import Data.Generics
import Data.Map (Map)
import Data.Set as S (Set, member, unions, insert, (\\), null)
import Data.List (intersperse)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Specification declaration
data Spec = Spec [Decl] -- ^ List of declarations
      deriving (Eq, Show, Data, Typeable)

-- | List of declarations
data Decl = Var [Id] -- ^ Set of variables
    | Theory [Thdecl] -- ^ Set of rules
    | Rules [Rule] -- ^ Set of rules
    | Strategy Strategy -- ^ Extra information
    | AnyList Id [String]
      deriving (Eq, Show, Data, Typeable)

-- | Theory declaration (para obligar a que haya min. uno??)
data Thdecl = Thdecl SimpleThdecl [Thdecl]
      deriving (Eq, Data, Typeable)

-- | Simple theory declaration
data SimpleThdecl = Id Id [Id]
    | Equations [Equation] 
      deriving (Eq, Data, Typeable)

-- | Equation declaration
data Equation = Term :==: Term -- ^ Equation
      deriving (Eq, Data, Typeable)

{-
-- | Equation declaration (para obligar a que haya min. uno??)
data Equation = Equation SimpleEquation [Equation]
      deriving (Eq, Data, Typeable)

-- | Simple equation declaration
data SimpleEquation = Term :==: Term -- ^ Equation
      deriving (Eq, Data, Typeable)
-}

-- | Term declaration
data Term = T Id [Term] -- ^ Term
    deriving (Eq, Data, Typeable)

-- | Rule declaration
data Rule = Rule SimpleRule [Cond] -- ^ Conditional rewriting rule
      deriving (Eq, Data, Typeable)

-- | Simple rule declaration
data SimpleRule = Term :-> Term  -- [Cond] -- Flecha Term Term [Cond] -- ^ Rewriting rule   
    | Term :->= Term -- [Cond] -- FlechaIgual Term Term [Cond] 
    deriving (Eq, Data, Typeable)

data Cond = Term :-><- Term --
    | Arrow Term Term -- | Term :-> Term
    deriving (Eq, Data, Typeable)

-- | Strategy Declaration
data Strategy = INNERMOST
  | OUTERMOST
  | CONTEXTSENSITIVE Csstratlist
    deriving (Eq, Show, Data, Typeable)

data Csstratlist = Csstratlist Id [Int] Csstratlist
    deriving (Eq, {-Show,-} Data, Typeable)


-- | Identifier
type Id = String

-- | TSR Type
data TRSType = TRSStandard
  | TRSConditional --CondType
  | TRSContextSensitive 
  | TRSContextSensitiveConditional --CondType
    deriving (Show)

-- | Term Rewriting Systems (TRS, CTRS, CSTRS, CSCTRS)
data TRS 
  = TRS { trsSignature :: Map Id Int
        , trsVariables :: Set Id
        , trsRMap :: [(Id, [Int])]
        -- , trsRules :: [Rule]
        , trsType :: TRSType
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

instance Show Term where
  show (T f []) = f 
  show (T f terms) = f ++ "(" ++ (concat . intersperse "," . map show $ terms) ++ ")" 

instance Show SimpleRule where 
  show (t1 :-> t2) = show t1 ++ " -> " ++ show t2
  show (t1 :->= t2) = show t1 ++ " ->= " ++ show t2
  
instance Show Rule where 
  show (Rule r []) = show r
  show (Rule r conds) = show r ++ " | " ++ (concat . intersperse ", " . map show $ conds)

instance Show Cond where
  show (t1 :-><- t2) = show t1 ++ " -><- " ++ show t2
  -- show (t1 :-> t2) = show t1 ++ " -> " ++ show t2

instance Show Csstratlist where
  show (Csstratlist id [] csstratlist) = "(" ++ id ++ ")" ++ map show $ csstratlist
  show (Csstratlist id nums csstratlist) = "(" ++ id ++ (concat . intersperse " " . map show $ nums) ++ ")" ++ show $ csstratlist

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | gets all the terms from a rule
getVars :: Set Id -> Term -> Set Id
getVars vs (T idt ts) = let tsVars = unions . map (getVars vs) $ ts
                        in if member idt vs then
                             insert idt tsVars 
                           else 
                             tsVars

{-

-- | gets all the terms from a rule
getTerms :: Rule -> [Term]
getTerms (Rule (l :-> r) eqs) = (l:r:concatMap getTermsEq eqs)

-}

-- | gets all the terms from a equation
getTermsEq :: Equation -> [Term]  -- getTermsEq :: SimpleEquation -> [Term]
getTermsEq (l :==: r) = [l,r]

{-

-- | checks if the lhs is non-variable
nonVarLHS :: Set Id -> Rule -> Bool
nonVarLHS vs (Rule ((T idt _) :-> r) eqs) = not . member idt $ vs 

-- | checks if the rule is conditional
isCRule :: Rule -> Bool
isCRule (Rule _ []) = False 
isCRule _ = True

-- | checks if the non-conditional rule has extra variables
hasExtraVars :: Set Id -> Rule -> Bool
hasExtraVars vs (Rule (l :-> r) []) = not . S.null $ getVars vs r \\ getVars vs l
hasExtraVars _ _ = error $ "Error: hasExtraVars only applies to non-conditional rules"

-}