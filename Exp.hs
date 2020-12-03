module Exp (GExp(..), variables)
where

import Data.List (nub)
import Data.Foldable (toList)

data GExp a
  = FALSE                   -- Constant False
  | TRUE                    -- Constant True
  | VAR  a                  -- Variable
  | NOT  (GExp a )          -- Negation
  | AND  (GExp a) (GExp a)  -- Conjunction
  | OR   (GExp a) (GExp a)  -- Disjunction
  | XOR  (GExp a) (GExp a)  -- Exclusive or
  | BIIM (GExp a) (GExp a)  -- Biimplication
  | IMPL (GExp a) (GExp a)  -- Conditional (=>)
  deriving (Eq)

instance Functor GExp where
  fmap f FALSE = FALSE
  fmap f TRUE  = TRUE
  fmap f (VAR v) = VAR (f v)
  fmap f (NOT e) = NOT (fmap f e)
  fmap f (AND  e1 e2) = AND  (fmap f e1) (fmap f e2)
  fmap f (OR   e1 e2) = OR   (fmap f e1) (fmap f e2)
  fmap f (XOR  e1 e2) = XOR  (fmap f e1) (fmap f e2)
  fmap f (BIIM e1 e2) = BIIM (fmap f e1) (fmap f e2)
  fmap f (IMPL e1 e2) = IMPL (fmap f e1) (fmap f e2)

instance Foldable GExp where
  foldr f acc TRUE  = acc
  foldr f acc FALSE = acc
  foldr f acc (VAR v) = f v acc
  foldr f acc (NOT e) = foldr f acc e
  foldr f acc (AND  e1 e2) = foldr f (foldr f acc e2) e1
  foldr f acc (OR   e1 e2) = foldr f (foldr f acc e2) e1
  foldr f acc (XOR  e1 e2) = foldr f (foldr f acc e2) e1
  foldr f acc (BIIM e1 e2) = foldr f (foldr f acc e2) e1
  foldr f acc (IMPL e1 e2) = foldr f (foldr f acc e2) e1

instance Show a => Show (GExp a) where
  show FALSE = "f"
  show TRUE  = "t"
  show (VAR v) = show v
  show (NOT e) = "(~"++show e++")"
  show (AND  e1 e2) = "(" ++ show e1 ++ " ∧ " ++ show e2 ++ ")"
  show (OR   e1 e2) = "(" ++ show e1 ++ " ∨ " ++ show e2 ++ ")"
  show (XOR  e1 e2) = "(" ++ show e1 ++ " ⊕ " ++ show e2 ++ ")"
  show (BIIM e1 e2) = "(" ++ show e1 ++ " ⇔ " ++ show e2 ++ ")"
  show (IMPL e1 e2) = "(" ++ show e1 ++ " ⇒ " ++ show e2 ++ ")"

variables :: Eq a => GExp a -> [a]
variables exp = nub $ toList exp
