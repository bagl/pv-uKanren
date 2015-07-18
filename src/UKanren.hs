{-# LANGUAGE TupleSections #-}

module UKanren where

import Control.Monad (mzero)
import Data.Monoid ((<>))

data Term = Var Index
          | Atom String
          | Pair Term Term
          deriving (Eq)

instance Show Term where
  show t = case t of
    Var v    -> show v
    Atom a   -> '\'' : a
    Pair l r -> "(" ++ show l ++ ", " ++ show r ++ ")"

type Index = Int
type Stream = [State]
type State = (Subs, Index)
type Subs = [(Term, Term)]
type Goal = State -> Stream

var :: Index -> Term
var = Var

emptyStream :: Stream
emptyStream = []

emptyState :: State
emptyState = ([], 0)

walk :: Term -> Subs -> Term
walk v@Var{} subs = maybe v (`walk` subs) $ lookup v subs
walk t       _    = t

extend :: Term -> Term -> Subs -> Subs
extend v a s = (v, a) : s

(===) :: Term -> Term -> Goal
(===) u v (s, c) = case unify u v s of
  Nothing -> emptyStream
  Just s' -> return (s', c)

unify :: Term -> Term -> Subs -> Maybe Subs
unify u v s = go (walk u s) (walk v s)
  where go u'@Var{} v'@Var{} | u' == v' = return s
        go u'@Var{} v'                  = return $ extend u' v' s
        go u'       v'@Var{}            = return $ extend v' u' s
        go (Pair u1 u2) (Pair v1 v2)    = unify u1 v1 s >>= unify u2 v2
        go u'       v'                  = if u' == v'
                                            then return s
                                            else mzero

fresh :: (Term -> Goal) -> State -> Stream
fresh f (s, c) = f (var c) (s, succ c)

disj :: Goal -> Goal -> Goal
disj g1 g2 sc = g1 sc <> g2 sc

conj :: Goal -> Goal -> Goal
conj g1 g2 sc = g1 sc >>= g2

aORb :: Goal
aORb = conj (fresh $ \a -> a === Atom "a")
            (fresh $ \b -> disj (b === Atom "5") (b === Atom "6"))

main :: IO ()
main = print $ aORb emptyState
