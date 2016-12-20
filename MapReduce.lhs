> {-# LANGUAGE UnicodeSyntax #-}
> module MapReduce
> where
> import Prelude hiding (Monoid)
> import Unicode
> import Hardware
> import Data.List

> class Monoid a where
>   ε    ∷  a
>   (•)  ∷  a → a → a

> reduce  ∷  (Monoid m) ⇒ [m] → m
> reduce  =  foldr (•) ε

instance Monoid Bool where

> newtype OrdList elem = Ord [elem]

instance (Ord elem) ⇒ Monoid (OrdList elem) where

foldm ∷ (a → a → a) → a → ([a] → a)

> kpg ∷ (Bit, Bit) → (Carry → Carry)
> kpg (O,  O  )  =  \ _c  → O  -- kill
> kpg (O,  I  )  =  \ c   → c  -- propagate
> kpg (I,  O  )  =  \ c   → c  -- propagate
> kpg (I,  I  )  =  \ _c  → I  -- generate

> data KPG  =  K | P | G

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 2.1
============

constTrue

00 1
01 1
10 1
11 1

constFalse

00 0
01 0
10 0
11 0

first

00 0
01 0
10 1
11 1

negateFirst

00 1
01 1
10 0
11 0

second

00 0
01 1
10 0
11 1

negateSecond

00 1
01 0
10 1
11 0

newtype Second = SecondCons Bool
instance Monoid Second where
     ε = ?
     x • y = y

Showing with this example:
For all of the above we cannot define a monoid as the ε depends on the input.

xOr / xNor

00 0 1
01 1 0
10 1 0
11 0 1

> newtype Xor = XorCons Bool
>               deriving(Show)
> instance Monoid Xor where
>      ε = XorCons False
>      (XorCons x) • (XorCons y)
>          | x == y = XorCons False
>          | otherwise = XorCons True

> newtype XNor = XNorCons Bool
>               deriving(Show)
> instance Monoid XNor where
>      ε = XNorCons True
>      (XNorCons x) • (XNorCons y)
>          | x == y = XNorCons True
>          | otherwise = XNorCons False

and / nAnd

00 0 1
01 0 1
10 0 1
11 1 0

> newtype AndMonoid = AndMonoid Bool
>     deriving(Show)

> instance Monoid AndMonoid where
>     ε = AndMonoid True
>     (•) (AndMonoid x) (AndMonoid y) = AndMonoid $ x && y

You cannot make a Monoid from nAnd because it is not associative.

(True nAnd False) nAnd False != True nAnd (False nAnd False)
True nAnd False != True nAnd True
True != False

or / nOr

00 0 1
01 1 0
10 1 0
11 1 0

> newtype Or = OrCons Bool
>             deriving(Show)
> instance Monoid Or where
>      ε = OrCons False
>      (OrCons x) • (OrCons y) = OrCons (x || y)

nOr is not possible because it is not associative
(True nOr False) nOr False != True nOr (False nOr False)

implicatie / nImplicatie

00 1 0
01 1 0
10 0 1
11 1 0

Implication is not associative so we cannot make a monoid from it.

False -> _ = True
(False -> _) -> False = False

The same is true for the negated implication.

onlyFirstBit / nOnlyFirstBit

00 0 1
01 1 0
10 0 1
11 0 1

onlyFirstBit is not possible since there exists no ε where a • ε = a
We cannot choose either True or False since it depends on the input

exercise 2.2
============

For the following functions there is already a function predefined:
And: and
Or: or

exercise 3
==========

> newtype OrderedListMonoid a = OrderedListMonoid [a]
>     deriving(Show)

> instance (Ord a) => Monoid (OrderedListMonoid a) where
>     ε = OrderedListMonoid []
>     (OrderedListMonoid a) • (OrderedListMonoid b)
>         = OrderedListMonoid $ merge a b

> merge :: (Ord a) => [a] -> [a] -> [a]
> merge [] [] = []
> merge a [] = a
> merge [] a = a
> merge (a:as) (b:bs)
>     | a < b = a : (merge as (b:bs))
>     | otherwise = b : (merge (a:as) bs)

Yes, you can use monoids to implement mergesort. You can use the merge function
defined above.

> newtype OrderMonoid a = OrderMonoid [a]
>     deriving(Show)

> instance (Ord a) => Monoid (OrderMonoid a) where
>     ε = OrderMonoid []
>     (OrderMonoid a) • (OrderMonoid b)
>         = OrderMonoid $ merge a b

exercise 4.1
============

The predefined functions foldr and foldl in Haskell are more generic than our
foldm. We define foldm on lists but foldl and foldr are defined for Foldables
which include Lists and other things.

The other difference is that the elements for foldl and foldr can be of another
type than the result. This is the case because for foldr the result type b is
always on the right, for foldl the result type is always on the left.

For foldl the result is on both sides so the element and result type must be the
same.

exercise 4.2
============

exercise 4.3
============

exercise 5.1
============

exercise 5.2
============

exercise 5.3
============
