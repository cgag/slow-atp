{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Text.PrettyPrint
import Data.List
import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.String
import Data.Text (Text)
import qualified Data.Text  as T
-- import Data.Text
import Failing
import Control.Monad (foldM)

data PrologRule =
  Prolog (S.Set (Formula FOL)) (Formula FOL)
  deriving (Eq, Ord)

data Formula a = FF
               | TT
               | Atom !a
               | Not !(Formula a)
               | And !(Formula a) !(Formula a)
               | Or  !(Formula a) !(Formula a)
               | Imp !(Formula a) !(Formula a)
               | Iff !(Formula a) !(Formula a)
               | Forall V !(Formula a)
               | Exists V !(Formula a)
               deriving (Eq, Ord)

data Term =
    Var !V
  | Fn !F !(V.Vector Term)
  deriving (Eq, Ord)

data FOL =
  R !P !(V.Vector Term)
  deriving (Eq, Ord)

newtype V = V Text deriving (Eq, Ord, Generic)
instance Hashable V

instance IsString V where
    fromString = V . T.pack

newtype F = F Text deriving (Eq, Ord)

instance IsString F where
    fromString = F . T.pack

newtype P = P Text deriving (Eq, Ord)

instance IsString P where
    fromString = P . T.pack

tryfind :: (t -> Failing b) -> V.Vector t -> Failing b
tryfind f s
  | V.null s = failure "tryfind"
  | otherwise =  tryM (f (V.head s)) (tryfind f (V.tail s))

settryfind :: (t -> Failing b) -> S.Set t -> Failing b
settryfind f s =
    case S.minView s of
      Nothing -> failure "tryfind"
      Just (h, t) -> tryM (f h) (settryfind f t) -- either (const (tryfind f t)) return (f h)

setToSeq :: S.Set a -> V.Vector a
setToSeq = foldr V.cons mempty . S.toAscList

setFromSeq :: Ord a => V.Vector a -> S.Set a
setFromSeq = foldr S.insert mempty

setUnions :: Ord a => V.Vector (S.Set a) -> S.Set a
setUnions = foldr S.union mempty
