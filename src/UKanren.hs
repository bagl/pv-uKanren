module UKanren where

import           Control.Monad (MonadPlus, mzero, mplus)
import qualified Data.Map as M
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

data DStream a = Empty
               | Delay (DStream a)
               | a ::: DStream a
               deriving (Show)

instance Monad DStream where
  return = (::: Empty)
  Empty     >>= _ = Empty
  (Delay s) >>= f = Delay $ s >>= f
  (a ::: s) >>= f = f a `mplus` (s >>= f)

instance MonadPlus DStream where
  mzero               = Empty
  mplus s1      Empty = s1
  mplus Empty      s2 = s2
  mplus (Delay s1) s2 = Delay $ mplus s2 s1
  mplus (s ::: s1) s2 = s ::: mplus s2 s1

-- return a >>= k = k a
--
--     a ::: Empty >>= k
--     = k a `mplus` (Empty >>= f)
--     = k a `mplus` Empty
--     = k a
--     QOD
--
-----------------------------------------------------------------------------
-- m >>= return = m
-----------------------------------------------------------------------------
--
-- Empty >>= (::: Empty)
-- = Empty
--
-- Delay s >>= (::: Empty)
-- = Delay $ s >>= (::: Empty)
-- ** if s /= Delay    => Delay $ s >>= return      = Delay $ s = Delay s -- QOD
--                                (assume it holds)
--
-- ** if s == Delay s' => Delay $ Delay $ s' >>= return -- ad inf
--
-- (a ::: s) >>= (::: Empty)
-- = (a ::: Empty) `mplus` (s >>= (::: Empty))
-- = a ::: (mplus Empty (s >>= (::: Empty)))
-- = a ::: (s >>= (::: Empty))
-- ** if s /= :::       => a ::: s -- QOD
-- ** if s == a' ::: s' => a ::: (a' ::: Empty (`mplus` (s' >>= return)))
--                      == a ::: (a' ::: (Empty `mplus` (s' >>= return)))
--                      == a ::: a' ::: (s' >>= return) -- ad inf
--
-----------------------------------------------------------------------------
-- m >>= (\x -> k x >>= h) == (m >>= k) >>= h
-----------------------------------------------------------------------------
--
-- Empty >>= (\x -> k x >>= h)        == Empty
-- (Empty >>= k) >>= h == Empty >>= h == Empty   -- QOD

-- (Delay s >>= k) >>= h == (Delay $ s >>= k) >>= h
--                       == Delay $ (s >>= k) >>= h
-- Dealy s >>= (\x -> k x >>= h)  == Delay $ s >>= (\x -> k x >>= h)
--            ** if s /= Delay    => Delay $ (s >>= k) >>= h
--            ** if s == Delay s' => Delay $ Delay $ s' >>= (\x -> k x >>= h) -- ad inf
-- 
-- a ::: s >>= (\x -> k x >>= h) == (k a >>= h) `mplus` (s >>= (\x -> k x >>= h))
-- (a ::: s >>= k) >>= h == (k a `mplus` (s >>= k)) >>= h
--   ** k a == Empty => (Empty >>= h) `mplus` (s >>= (\x -> k x >>= h))
--                   == Empty `mplus` (s ....)
--                   == s >>= (\x -> k x >> h)
--                   => (Empty `mplus` (s >>= k)) >>= h
--                   == (s >>= k) >>= h -- QED
--
--   ** k a == Delay => (Delay b >>= h) `mplus` (s >>= ...)
--                   == (Delay $ b >>= h) `mplus` (s >>= ...)
--                   == Delay $ mplus (b >>= h) (s >>= ...)
--                   => (Delay b `mplus` (s >>= k)) >>= h
--                   == (Delay $ mplus b (s >>= k)) >>= h
--                   == Delay $ (mplus b (s >>= k)) >>= h
--                   == ???

streamToList :: DStream a -> [a]
streamToList Empty = []
streamToList (Delay s) = streamToList s
streamToList (a ::: s) = a : streamToList s

type VLabel = String
type Stream = DStream State
type State = M.Map Term Term
type Goal = State -> Stream

var :: VLabel -> Term
var = Var

emptyStream :: Stream
emptyStream = Empty

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
  Just s' -> s' ::: Empty

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
disj g1 g2 sc = g1 sc `mplus` g2 sc

zzz :: Goal -> Goal
zzz g s = Delay $ g s

conj :: Goal -> Goal -> Goal
conj g1 g2 sc = g1 sc >>= g2

aORb :: Goal
aORb = conj (fresh "a" $ \a -> a === Atom "a")
            (fresh "b" $ \b -> disj (b === Var "a") (b === Atom "6"))

inf56 :: Goal
inf56 = fresh "x" $ \x -> disj (r5 x) (r6 x)
  where r5 x = disj (x === Atom "5") (zzz $ r5 x)
        r6 x = disj (x === Atom "6") (zzz $ r6 x)

five :: Goal
five = fresh "x" $ \x ->  x === Atom "5"
fives_ :: Term -> Goal
fives_ x = disj (x === Atom "5") (zzz $ fives_ x)
fives :: Goal
fives = fresh "y" fives_
 
fivesRev_ :: Term -> Goal
fivesRev_ x = disj (zzz $ fivesRev_ x) (x === Atom "5")
fivesRev :: Goal
fivesRev = fresh "y" fivesRev_

main :: IO ()
main = do
  print $ aORb emptyState
  print $ take 10 $ streamToList $ inf56 emptyState
  print $ take 10 $ streamToList $ fives    emptyState
  print $ take 10 $ streamToList $ fivesRev emptyState
