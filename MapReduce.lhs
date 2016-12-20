> {-# LANGUAGE UnicodeSyntax #-}
> module MapReduce
> where
> import Prelude hiding (Monoid)
> import Unicode
> import Hardware

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

xOr / xNor

00 0 1
01 1 0
10 1 0
11 0 1

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

exercise 2.2
============

exercise 3
==========

exercise 4.1
============

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
