{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
This modle contains a number of classes for building constraints of one variable.

Whilst this package can stand alone, it was developed to be used in conjuction with the package "polydata".
Hence the documentation here is brief and examples (and indeed test cases currently) are more from the package "polydata".
-}
module Control.ConstraintManip (
  type (&&&),
  And,
  -- $getArgDocs
  Arg,
  Result,
  GetArg,
  IxIs,
  -- $restClassDoc
  IxEqual,
  IxConstrainBy,
  IxConstrainPair,
  IxConstructorEqual,
  IxConstraintedConstructorEqual,
  )
where

import GHC.TypeLits (Nat)

import Control.IndexT (IndexT)
import Control.IndexT.Function (ResultT, IsFunc, IsHomoFunc, IsHomoArgFunc)
import Control.IndexT.Tuple (IsTuple, IsHomoTuple)

{- $getArgDocs
The following types 'Arg' and 'Result', and 'GetArg' are used to reference particular arguments of functions.

It's worth remembering `Arg` is zero based, so the first argument is @Arg 0@.

With 'Result', you still need to specify the number of arguments. So

> GetArg (Result 2) (t1 -> t2 -> t3) == t3

Naturally:

> GetArg (Result 1) (t1 -> t2 -> t3) == t2 -> t3

as

> (t1 -> t2 -> t3) == (t1 -> (t2 -> t3))
-}
data Arg (n :: Nat)
data Result (n :: Nat)

type family GetArg t a where
  GetArg (Arg n) a = IndexT n a
  GetArg (Result n) a = ResultT n a

{-|
This class combines two constraints. Given:

> c1 :: * -> Constraint
> c2 :: * -> Constraint

then

> (c1 &&& c2) :: * -> Constraint

is only satisfied when both @c1@ and @c2@ are satisfied. This is useful for building constraints.
-}
class (c1 a, c2 a) => (&&&) c1 c2 a
instance (c1 a, c2 a) => (&&&) c1 c2 a

class (c1 a, c2 a) => And c1 c2 a
instance (c1 a, c2 a) => And c1 c2 a
{-|
> IxIs i t

is a constraint that says element @i@ is @t@.

For example:

> IxIs (Arg 1) Int

is of type @* -> Constraint@ such that

> IxIs (Arg 1) Int t

says that second argument of @t@ is Int. Remember that for this library, function args are 0 based.

> IxIs (Result 2) Int t

says the result of the two argument function is Int. The definition is:

> IxIs i t a == GetArg i a ~ t
-}
class (GetArg i a ~ t) => IxIs i t a
instance (GetArg i a ~ t) => IxIs i t a

{- $restClassDocs
These following functions are similar in structure to 'IxIs'.
-}

{-|
@IxConstrainBy i c a@ applies constraint @c@ to the ith argument. i.e.

> IxConstrainBy i c a == c (GetArg i a)
-}
class (c (GetArg i a)) => IxConstrainBy i c a
instance (c (GetArg i a)) => IxConstrainBy i c a

{-|
@IxConstrainPair c i1 i2@ applies two argument constraint @c@ to the @i1@th and @i2@th element in that order. i.e.

> IxConstrainPair c i1 i2 a == c (GetArg i1 a) (GetArg i2 a)
-}
class (c (GetArg i1 a) (GetArg i2 a)) => IxConstrainPair c i1 i2 a
instance (c (GetArg i1 a) (GetArg i2 a)) => IxConstrainPair c i1 i2 a

{-|
@IxEqual i1 i2 a@ just says arguments @i1@ and @i2@ are equal. i.e.

> IxEqual i1 i2 a == GetArg i1 a ~ GetArg i2 a
-}
class (GetArg i1 a ~ GetArg i2 a) => IxEqual i1 i2 a
instance (GetArg i1 a ~ GetArg i2 a) => IxEqual i1 i2 a

{-
@IxConstructorEqual i1 f i2 a@ says @i1@ is just @i2@ but with @f@ constructor applied to it. i.e.

> IxEqual i1 i2 a == GetArg i1 a ~ f (GetArg i2 a)
-}
class (GetArg i1 a ~ f (GetArg i2 a)) => IxConstructorEqual i1 f i2 a
instance (GetArg i1 a ~ f (GetArg i2 a)) => IxConstructorEqual i1 f i2 a

{-
@IxConstraintedConstructorEqual i1 f i2 a@ says @i1@ is just @i2@ but with @f@ constructor applied to it. i.e.

> IxEqual i1 i2 a == GetArg i1 a ~ GetArg i2 a
-}
class (GetArg i1 a ~ f (GetArg i2 a), c f) => IxConstraintedConstructorEqual i1 c f i2 a
instance (GetArg i1 a ~ f (GetArg i2 a), c f) => IxConstraintedConstructorEqual i1 c f i2 a
