{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, TupleSections #-}

module Main where

import Control.Applicative
import Data.Fix
import Data.Foldable (fold)
import Data.Functor.Compose
import Data.Set hiding (fold)
import Prelude hiding (foldr, map)

newtype Iden = Iden Int
  deriving (Eq, Ord, Read, Show)

data FormF a =
  Lit Bool |
  Var Iden |
  Let Iden Bool a |
  a :|| a |
  a :&& a |
  a :-> a
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

type Form = Fix FormF

(-->) :: Bool -> Bool -> Bool
False --> _ = True
_ --> b = b

-- This is minimal.
freeAlg :: FormF (Set Iden) -> Set Iden
freeAlg =
  let f (Var i) = singleton i
      f x = fold x in
      f

free :: Form -> Set Iden
free = cata freeAlg

type CompF a = Compose ((,) a) FormF a

substCoalg :: Form -> CompF Form
substCoalg =
  let f y @ (Fix x) = Compose (y, x) in
      f

substAlg :: Iden -> Bool -> CompF Form -> Form
substAlg i' b' =
  let f (Compose (_, x @ (Var i)))
        | i == i' = Fix (Lit b')
        | otherwise = Fix x
      f (Compose (y, x @ (Let i _ _)))
        | i == i' = y
        | otherwise = Fix x
      f (Compose (_, x)) = Fix x in
      f

subst :: Iden -> Bool -> Form -> Form
subst i b = hylo (substAlg i b) substCoalg

eval :: Form -> Either Form Bool
eval =
  let f (Fix (Lit b)) = Right b
      f (Fix (x @ (Var _))) = Left (Fix x)
      f (Fix (Let i b x)) = f (subst i b x)
      f (Fix (x :|| y)) = (||) <$> f x <*> f y
      f (Fix (x :&& y)) = (&&) <$> f x <*> f y
      f (Fix (x :-> y)) = (-->) <$> f x <*> f y in
      f

simplDisjAlg :: FormF Form -> Form
simplDisjAlg =
  let f (x @ (Fix (Lit True)) :|| _) = x
      f (_ :|| y @ (Fix (Lit True))) = y
      f (Fix (Lit False) :|| y) = y
      f (x :|| Fix (Lit False)) = x
      f x = Fix x in
      f

simplDisj :: Form -> Form
simplDisj = cata simplDisjAlg

simplConjAlg :: FormF Form -> Form
simplConjAlg =
  let f (x @ (Fix (Lit False)) :&& _) = x
      f (_ :&& y @ (Fix (Lit False))) = y
      f (Fix (Lit True) :&& y) = y
      f (x :&& Fix (Lit True)) = x
      f x = Fix x in
      f

simplConj :: Form -> Form
simplConj = cata simplConjAlg

-- This is fast.
simpl :: Form -> Form
simpl = cata (simplConjAlg . unFix . simplDisjAlg)

test :: Form
test =
  let x : y : z : _ = Iden <$> [1 ..] in
      Fix (Fix (Lit False) :|| Fix (Fix (Var x) :&&
      Fix (Let y False
               (Fix (Fix (Lit True) :-> Fix (Fix (Var y) :||
               Fix (Let x True
                        (Fix (Fix (Var x) :&& Fix (Var z))))))))))

main :: IO ()
main =
  do let simple = simpl test
         idens = free simple
         binds = map (, True) idens
         form = foldr (uncurry subst) simple binds
         result = eval form
     print test
     print simple
     print idens
     print binds
     print form
     print result
