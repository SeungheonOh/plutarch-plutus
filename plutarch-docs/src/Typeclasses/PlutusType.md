# `PlutusType`

## Prerequisites

[TODO: Fill in]

This is a description oriented towards the 'big picture' view and practicalities
of how to implement instances of `PlutusType`. For a more detailed explanation
of what exactly `PlutusType` does, please see the MLabs blog article [_From Term 
to Script: How PlutusType Drives Plutarch_][term-to-script].

## Anatomy of `PlutusType`

[TODO: Lead in]

```haskell
-- Not a complete description, but enough to explain everything we will need
class PlutusType (a :: S -> Type) where
    type PInner a :: S -> Type
    pcon' :: forall (s :: S) . a s -> Term s (PInner a)
    pmatch' :: forall (s :: S) (b :: S -> Type) . Term s (PInner a) -> (a s ->
    Term s b) -> Term s b
```

## Purpose of the class

## Laws

## Interface

The two 'directions' of `PlutusType` give rise to two parallel interfaces: one
to sequence and lift computations in `Term`s, the other to convert from `a` to
`PInner a` at (essentially) no cost. For convenience, there are helpers
available to make this easier, as using `PlutusType`'s methods directly can be a
bit awkward.

While you can use `pcon'` directly to introduce `Term`s, and `pmatch'` directly
to sequence `Term` computations, dealing with `PInner` in this context is
usually not useful. Thus, Plutarch provides the following helpers:

```haskell
-- Only signatures, as the bodies don't matter
pcon :: forall (a :: S -> Type) (s :: S) . PlutusType a => a s -> Term s a

pmatch :: forall (a :: S -> Type) (s :: S) (b :: S -> Type) . 
    PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
```

We can use `pcon` to introduce `Term`s of a particular representation:

```haskell
aNothing :: forall (a :: S -> Type) (s :: S) . PlutusType a => Term s (PMaybe a)
aNothing = pcon PNothing
```

and `pmatch` to sequence them, together with pattern matching if useful:

```haskell
-- CaseLambda is quite useful here
pmapMaybe :: forall (a :: S -> Type) (b :: S -> Type) (s :: S) . 
    (PlutusType a, PlutusType b) =>
    Term s (a :--> b) -> 
    Term s (PMaybe a) -> 
    Term s (PMaybe b)
pmapMaybe f x = pmatch x $ \case
    PNothing -> pcon PNothing
    PJust y -> pcon . PJust $ f # y
```

[TODO: Link?]

As (the representations of) `PInner a` and `a` are identical at runtime,
converting between them amounts to informing Plutarch to treat a given term
differently. This has (essentially) no cost, as it generates no code that needs
executing. To assist with this, Plutarch provides the following function:

```haskell
-- Actual definition is more general than this
pupcast :: forall (a :: S -> Type) (s :: S) . PlutusType a => Term s a -> Term s (PInner a)
```

This is essentially a 'safe' version of `punsafeCoerce`.

[term-to-script]: https://www.mlabs.city/blog/from-term-to-script-how-plutustype-drives-plutarch

