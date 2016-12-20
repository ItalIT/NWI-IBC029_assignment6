> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance Functor Tree where
>   fmap _f Empty         =  Empty
>   fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

> ex1  ∷  Tree Integer
> ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
> ex2  ∷  Tree String
> ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
> ex3  ∷  Tree Char
> ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

size ∷ Tree elem → Int
minHeight, maxHeight ∷ Tree elem → Int
member ∷ (Eq elem) ⇒ elem → Tree elem → Bool
preorder, inorder, postorder ∷ Tree elem → [elem]
layout ∷ (Show elem) => Tree elem → String
build ∷ [elem] → Tree elem
balanced ∷ [elem] → Tree elem
create ∷ Int → Tree ()

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 1
==========

> instance (Eq elem) => Eq (Tree elem) where
>     Empty == Empty = True
>     (Node _ _ _) == Empty = False
>     Empty == (Node _ _ _) = False
>     (Node l0 k0 r0) == (Node l1 k1 r1) = k0 == k1 && l0 == l1 && r0 == r1

> instance (Ord elem) => Ord (Tree elem) where
>     compare Empty Empty = EQ
>     compare Empty (Node _ _ _) = LT
>     compare (Node _ _ _) Empty = GT
>     compare (Node l0 k0 r0) (Node l1 k1 r1)
>         | k0 == k1 = compare l0 l1
>         | otherwise = case compare k0 k1 of
>             LT -> LT
>             GT -> GT
>             EQ -> compare r0 r1

Trees, like anything, can be stored in Trees/Sets themselfs. If you store trees
in a binary search tree you must be able to compare and sort them.

Equality is well defined while the ordering is pretty arbitrary but that does
not matter to our data structures, they just need a total ordering. It does not
have to make sense, just be consistent.
