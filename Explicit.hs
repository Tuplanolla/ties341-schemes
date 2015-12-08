{-# LANGUAGE TupleSections #-}

module Main where

import Data.Set
import Prelude hiding (foldr, map)

newtype Iden = Iden Int
  deriving (Eq, Ord, Read, Show)

data Form =
  Lit Bool |
  Var Iden |
  Let Iden Bool Form |
  Form :|| Form |
  Form :&& Form |
  Form :-> Form
  deriving (Eq, Read, Show)

(-->) :: Bool -> Bool -> Bool
False --> _ = True
_ --> b = b

free :: Form -> Set Iden
free =
  let f (Lit _) = empty
      f (Var i) = singleton i
      f (Let i _ x) = i `delete` f x
      f (x :|| y) = f x `union` f y
      f (x :&& y) = f x `union` f y
      f (x :-> y) = f x `union` f y in
      f

subst :: Iden -> Bool -> Form -> Form
subst i' b' =
  let f x @ (Lit _) = x
      f x @ (Var i)
        | i == i' = Lit b'
        | otherwise = x
      f x @ (Let i b y)
        | i == i' = x
        | otherwise = Let i b (f y)
      f (x :|| y) = f x :|| f y
      f (x :&& y) = f x :&& f y
      f (x :-> y) = f x :-> f y in
      f

eval :: Form -> Either Form Bool
eval =
  let f (Lit b) = Right b
      f x @ (Var _) = Left x
      f (Let i b x) = f (subst i b x)
      f (x :|| y) = (||) <$> f x <*> f y
      f (x :&& y) = (&&) <$> f x <*> f y
      f (x :-> y) = (-->) <$> f x <*> f y in
      f

simplDisj :: Form -> Form
simplDisj =
  let f x @ (Lit _) = x
      f x @ (Var _) = x
      f (Let i b x) = Let i b (f x)
      f (x @ (Lit True) :|| _) = x
      f (_ :|| y @ (Lit True)) = y
      f (Lit False :|| y) = f y
      f (x :|| Lit False) = f x
      f (x :|| y) = f x :|| f y
      f (x :&& y) = f x :&& f y
      f (x :-> y) = f x :-> f y in
      f

simplConj :: Form -> Form
simplConj =
  let f x @ (Lit _) = x
      f x @ (Var _) = x
      f (Let i b x) = Let i b (f x)
      f (x :|| y) = f x :|| f y
      f (x @ (Lit False) :&& _) = x
      f (_ :&& y @ (Lit False)) = y
      f (Lit True :&& y) = f y
      f (x :&& Lit True) = f x
      f (x :&& y) = f x :&& f y
      f (x :-> y) = f x :-> f y in
      f

-- This is slow.
simpl :: Form -> Form
simpl = simplConj . simplDisj

test :: Form
test =
  let x : y : z : _ = Iden <$> [1 ..] in
      Lit False :|| (Var x :&&
      Let y False
          (Lit True :-> (Var y :||
          Let x True
              (Var x :&& Var z))))

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
