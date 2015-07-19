module UKanren where

import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S

data Term = Var VLabel
          | Atom String
          | Pair Term Term
          deriving (Eq, Ord)

instance Show Term where
  show t = case t of
    Var v    -> show v
    Atom a   -> '\'' : a
    Pair l r -> "(" ++ show l ++ ", " ++ show r ++ ")"

type VLabel = String
type Stream = [State]
type State = M.Map Term Term
type Goal = State -> Stream

var :: VLabel -> Term
var = Var

emptyStream :: Stream
emptyStream = []

emptyState :: State
emptyState = M.empty

walk :: Term -> State -> Term
walk t s = go S.empty t
  where go visited v@Var{}
          | S.member v visited = error $ "loop in walk!\nvar: " ++ show v ++ "\nstate: " ++ show s
          | otherwise          = maybe v (`walk` s) $ M.lookup v s
        go _ t' = t'

extend :: Term -> Term -> State -> State
extend = M.insert

(===) :: Term -> Term -> Goal
(===) u v s = case unify u v s of
  Nothing -> emptyStream
  Just s' -> return s'

unify :: Term -> Term -> State -> Maybe State
unify u' v' s = go (walk u' s) (walk v' s)
  where go u@Var{} v@Var{} | u == v  = Just s
        go u@Var{} v                 = Just $ extend u v s
        go u       v@Var{}           = Just $ extend v u s
        go (Pair u1 u2) (Pair v1 v2) = unify u1 v1 s >>= unify u2 v2
        go u       v                 = if u == v
                                         then Just s
                                         else Nothing

fresh :: VLabel -> (Term -> Goal) -> State -> Stream
fresh v f = f (var v)

disj :: Goal -> Goal -> Goal
disj g1 g2 sc = g1 sc <> g2 sc

conj :: Goal -> Goal -> Goal
conj g1 g2 sc = g1 sc >>= g2

aORb :: Goal
aORb = conj (fresh "a" $ \a -> a === Atom "a")
            (fresh "b" $ \b -> disj (b === Var "a") (b === Atom "6"))

main :: IO ()
main = print $ aORb emptyState
