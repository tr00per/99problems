-- Solutions to "99 Prolog Problems" (c) 2016 Artur Czajka. 2-clause BSD license applies.

module Logic where

import           Control.Arrow
import           Data.List     (intercalate)
import           Data.Map      ((!))
import qualified Data.Map      as M
import           Prelude       hiding (and, not, or)
import qualified Prelude       as P

type Identifier = Char
type Possibilities = M.Map Identifier Bool
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

tab :: Bool -> String
tab False = "fail"
tab True = "true"

table :: [Identifier] -> Expr -> [String]
table vars expression = map (display . (eval expression &&& id)) combinations
    where
        display :: (Bool, Possibilities) -> String
        display (result, pm) =
            unwords (map (tab . (pm !)) vars) ++ " " ++ tab result

        eval :: Expr -> Possibilities -> Bool
        eval e comb = go e
            where
                go (Var v) = comb ! v
                go (And p q) = go p && go q
                go (Or p q) = go p || go q
                go (Not p) = P.not (go p)

        combinations = map (M.fromList . zip vars) (booleans $ length vars)

        booleans 0 = return []
        booleans n = do
            h <- [False, True]
            t <- booleans (n-1)
            return (h : t)

printTable :: [String] -> IO ()
printTable = putStrLn . unlines

-- and(A,or(A,B))
problem01 = printTable $ table ['A', 'B'] $ and (var 'A') (or (var 'A') (var 'B'))

-- A and (A or not B)
problem02 = printTable $ table ['A', 'B'] $ var 'A' `and` (var 'A' `or` not (var 'B'))

-- A and (B or C) equ A and B or A and C
problem03 = printTable $ table ['A', 'B', 'C'] $
    (var 'A' `and` (var 'B' `or` var 'C')) `equ` ((var 'A' `and` var 'B') `or` (var 'A' `and` var 'C'))
