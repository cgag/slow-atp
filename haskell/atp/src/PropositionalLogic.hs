{-# LANGUAGE OverloadedLists #-}
module PropositionalLogic where

import Prelude hiding (negate,sum)
import qualified Data.Set as S
import Data.List (intercalate,minimumBy,maximumBy)
import Data.Maybe
import qualified Data.HashMap.Strict as M
import GHC.Unicode(isDigit)
import Types
import Failing

simpcnf :: Ord t => Formula t -> S.Set (S.Set (Formula t))
simpcnf FF = S.singleton S.empty
simpcnf TT = S.empty
simpcnf fm = let cjs = S.filter (not . trivial) (purecnf fm) in
             S.filter (\c -> S.null (S.filter (flip S.isProperSubsetOf c) cjs)) cjs

purecnf :: Ord t => Formula t -> S.Set (S.Set (Formula t))
purecnf fm = S.map (S.map negate) (purednf (nnf (Not fm)))

simpdnf :: Ord t => Formula t -> S.Set (S.Set (Formula t))
simpdnf FF = S.empty
simpdnf TT = S.singleton S.empty
simpdnf fm = let djs = S.filter (not . trivial) (purednf (nnf fm)) in
             S.filter (\d -> S.null (S.filter (flip S.isProperSubsetOf d) djs)) djs

trivial :: Ord t => S.Set (Formula t) -> Bool
trivial lits = not (S.null (S.intersection pos (S.map negate neg)))
 where
  (pos,neg) = S.partition positive lits

purednf :: Ord t => Formula t -> S.Set (S.Set (Formula t))
purednf (And p q) = distrib (purednf p) (purednf q)
purednf (Or p q) = S.union (purednf p) (purednf q)
purednf fm = S.singleton (S.singleton fm)

allpairs :: (Ord c) => (a -> b -> c) -> S.Set a -> S.Set b -> S.Set c
allpairs f xs ys = S.fold (\ x zs -> S.fold (\ y zs' -> S.insert (f x y) zs') zs ys) mempty xs

distrib :: Ord a => S.Set (S.Set a) -> S.Set (S.Set a) -> S.Set (S.Set a)
distrib s1 s2 = allpairs (S.union) s1 s2

list_conj :: Foldable t => t (Formula a) -> Formula a
list_conj fs
  | null fs = TT
  | otherwise = foldr1 And fs

-- Convert to negation normal form
nnf :: Formula a -> Formula a
nnf fm = nnf' (psimplify fm)
 where
  nnf' (And p q) = And (nnf p) (nnf q)
  nnf' (Or p q) = Or (nnf p) (nnf q)
  nnf' (Imp p q) = Or (nnf (Not p)) (nnf q)
  nnf' (Iff p q) = Or (And (nnf p) (nnf q)) (And (nnf (Not p)) (nnf (Not q)))
  nnf' (Not (Not p)) = nnf p
  nnf' (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))
  nnf' (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))
  nnf' (Not (Imp p q)) = And (nnf p) (nnf (Not q))
  nnf' (Not (Iff p q)) = Or (And (nnf p) (nnf (Not q))) (And (nnf (Not p)) (nnf q))
  nnf' fm = fm

negate :: Formula t -> Formula t
negate (Not p) = p
negate p = Not p

negative :: Formula t -> Bool
negative (Not p) = True
negative _ = False

positive :: Formula t -> Bool
positive = not . negative

psimplify :: Formula a -> Formula a
psimplify (Not p) = psimplify1 (Not (psimplify p))
psimplify (And p q) = psimplify1 (And (psimplify p) (psimplify q))
psimplify (Or p q) = psimplify1 (Or (psimplify p) (psimplify q))
psimplify (Imp p q) = psimplify1 (Imp (psimplify p) (psimplify q))
psimplify (Iff p q) = psimplify1 (Iff (psimplify p) (psimplify q))
psimplify fm = fm

psimplify1 :: Formula a -> Formula a
psimplify1 (Not FF) = TT
psimplify1 (Not TT) = FF
psimplify1 (Not (Not p)) = p
psimplify1 (And p FF) = FF
psimplify1 (And FF p) = FF
psimplify1 (And p TT) = p
psimplify1 (And TT p) = p
psimplify1 (Or p FF) = p
psimplify1 (Or FF p) = p
psimplify1 (Or p TT) = TT
psimplify1 (Or TT p) = TT
psimplify1 (Imp FF p) = TT
psimplify1 (Imp p TT) = TT
psimplify1 (Imp TT p) = p
psimplify1 (Imp p FF) = Not p
psimplify1 (Iff p TT) = p
psimplify1 (Iff TT p) = p
psimplify1 (Iff p FF) = Not p
psimplify1 (Iff FF p) = Not p
psimplify1 fm = fm

atom_union :: (Ord a, Ord b) => (a -> b) -> Formula a -> S.Set b
atom_union f fm = S.map f (overatoms (S.insert) fm mempty)

onatoms :: (t -> Formula a) -> Formula t -> Formula a
onatoms _ TT = TT
onatoms _ FF = FF
onatoms f (Atom a) = f a
onatoms f (Not p) = Not (onatoms f p)
onatoms f (And p q) = And (onatoms f p) (onatoms f q)
onatoms f (Or p q) = Or (onatoms f p) (onatoms f q)
onatoms f (Imp p q) = Imp (onatoms f p) (onatoms f q)
onatoms f (Iff p q) = Iff (onatoms f p) (onatoms f q)
onatoms f (Forall x p) = Forall x (onatoms f p)
onatoms f (Exists x p) = Exists x (onatoms f p)

overatoms :: (t -> a -> a) -> Formula t -> a -> a
overatoms _ TT b = b
overatoms _ FF b = b
overatoms f (Atom a) b = f a b
overatoms f (Not p) b = overatoms f p b
overatoms f (And p q) b = overatoms f p (overatoms f q b)
overatoms f (Or p q) b = overatoms f p (overatoms f q b)
overatoms f (Imp p q) b = overatoms f p (overatoms f q b)
overatoms f (Iff p q) b = overatoms f p (overatoms f q b)
overatoms f (Forall x p) b = overatoms f p b
overatoms f (Exists x p) b = overatoms f p b
