-- Solutions to "99 Prolog Problems" (c) 2016 Artur Czajka. 2-clause BSD license applies.

module Logic where

import           Prelude hiding (and, not, or)

type Identifier = Char
data Expr =
    Var Identifier
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  deriving (Show, Read)

var :: Identifier -> Expr
not :: Expr -> Expr
and, or, nand, nor, impl, equ :: Expr -> Expr -> Expr
var = Var
and = And
or = Or
not = Not
nand p q = not (p `and` q)
nor p q = not (p `or` q)
impl p q = not p `or` q
equ p q = impl p q `and` impl q p

table :: [Identifier] -> Expr -> [String]
table = undefined

-- and(A,or(A,B))
problem01 = table ['A', 'B'] $ and (var 'A') (or (var 'A') (var 'B'))

-- A and (A or not B)
problem02 = table ['A', 'B'] $ var 'A' `and` (var 'A' `or` not (var 'B'))

-- A and (B or C) equ A and B or A and C
problem03 = table ['A', 'B', 'C'] $ (var 'A' `and` (var 'B' `or` var 'C')) `equ` ((var 'A' `and` var 'B') `or` (var 'A' `and` var 'C'))
