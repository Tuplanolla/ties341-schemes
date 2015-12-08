module Main where

{-
The following types are isomorphic.

    (a -> b -> b) -> b -> [a] -> b
    ((a, b) -> b) -> b -> [a] -> b
    ((a, b) -> b, b) -> [a] -> b
    (b, (a, b) -> b) -> [a] -> b
    (() -> b, (a, b) -> b) -> [a] -> b
    (Either () (a, b) -> b) -> [a] -> b
    (Maybe (a, b) -> b) -> [a] -> b
    (Maybe (a, b) -> b) -> ([a] -> b)

Reversing the arrows yields the following isomorphic types.

    (b -> Maybe (a, b)) -> (b -> [a])
    (b -> Maybe (a, b)) -> b -> [a]
-}

data List a = a :. List a | Empty
  deriving (Eq, Ord, Read, Show)

infixr 5 :.

foldR :: (Maybe (a, b) -> b) -> List a -> b
foldR f (x :. xs) = f (Just (x, foldR f xs))
foldR f Empty = f Nothing

unfoldR :: (b -> Maybe (a, b)) -> b -> List a
unfoldR f z =
  case f z of
       Just (x, y) -> x :. unfoldR f y
       Nothing -> Empty

data Colist a = a :.. Colist a
  deriving (Eq, Ord, Read, Show)

infixr 5 :..

cofoldR :: ((a, b) -> b) -> Colist a -> b
cofoldR f (x :.. xs) = f (x, cofoldR f xs)

counfoldR :: (b -> (a, b)) -> b -> Colist a
counfoldR f z =
  let (x, y) = f z in
      x :.. counfoldR f y

cut :: Int -> Colist a -> List a
cut =
  let f g n (x :.. xs)
        | n > 0 = f (g . (x :.)) (n - 1) xs
        | otherwise = g Empty in
      f id

main :: IO ()
main = print . cut 8 $ counfoldR (\ n -> (2 ^ n, n + 1)) 0
