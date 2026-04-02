# `Term` and what it does

We discuss `Term` and what it means in terms of Plutarch's workflow. [TODO:
Elaborate]

This is a brief description, focusing on the 'big picture' view. For a more
detailed explanation of the implementation of `Term`, please see the MLabs blog
article [_From Term to Script: How PlutusType Drives Plutarch_][term-to-script].

## Prerequisites

[TODO: Fill in]

## Anatomy of a `Term`

[TODO: Lead-in with `Term`'s tyvars]

As Plutarch is an eDSL for generating UPLC scripts, we fundamentally need a
_code generation monad_ to be available. This effectively acts as an
accumulator, building up the script we want in composable chunks. These chunks
need to be definable at any granularity we might want, ranging from a single
call to a builtin to potentially quite complex logic for a validator and
anything in-between. [TODO: More?]

`Term` fills this role in Plutarch. Put simply, `Term s a` is a description of
a UPLC computation which, when executed[^1], does one of the following:

1. Crashes with an error.
2. Succeeds producing the UPLC representation of `a`.

Thus, whenever you see a `Term`, you should imagine it as a segment of UPLC
code. The nearest analogy to this in 'basic' Haskell would be the `Q` monad used
for Template Haskell, although this is only a conceptual similarity[^2].

## The difference between `->` and `:-->` with `Term`

As `Term`s are embedded into Haskell, we can define functions on `Term`s using
the regular Haskell function arrow `->`. However, there is also a Plutarch
function arrow `:-->`, which is a representation of either builtins or UPLC
lambdas. Thus, we can have both

* `Term s a -> Term s b`; and
* `Term s (a :--> b)`

What is the difference? `Term s a -> Term s b` describes a _code
transformation_, which is performed by Plutarch: we take (the code of) a
computation that (possibly) produces (the representation of) `a`, and then
produce (the code of) a new computation that (possibly) produces (the
representation of) a `b`. No UPLC evaluation takes place when we apply an
argument here. 

On the other hand, `Term s (a :--> b)` is a computation which, when evaluated,
will produce some UPLC term that can have a UPLC argument applied to it. Note
that here, no _code_ transformation is taking place: we're describing a
computation in UPLC that can be applied. This might be a lambda or a builtin or
a partial application of either, but the idea is that this is happening at
_runtime_, rather than 'compile time' as per the above.

To make this distinction clearer, let us consider two examples. The first
should seem familiar:

```haskell
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
````

This is a code transformation: by taking a computation that results in a `PBool`
(which is the representation of UPLC booleans), as well as a 'then' and 'else'
computation, we get a new computation. This new computation would do something
akin to the following:

1. Evaluate the `PBool`-resulting computation. If that errors, error out.
2. If the result of Step 1 was (the representation of) `True`, then behave like
   the second argument computation.
3. Otherwise, behave like the third argument computation.

We can see here that the result of `pif` does not necessarily execute all the
'argument' computations when we run the UPLC it would produce. This is exactly
why we have `pif` as a code transformation. To compare, let's consider the
signature in Plutarch that the `IfThenElse` builtin operation would have:

```haskell
pite :: Term s (PBool :--> a :--> a :--> a)
```

[TODO: fill in]

## What about the `s`?

So far, we've been ignoring the `s` type argument to `Term`. It seems a mixture
of confusing and superfluous, especially if we examine the kind of `Term`:

```
-- This is Haskell's syntax for explicit kind signatures
type Term :: S -> (S -> Type) -> Type
```

Here, things get even more confusing: what on earth is this kind `S`?

[TODO: Elaborate]

[term-to-script]: https://www.mlabs.city/blog/from-term-to-script-how-plutustype-drives-plutarch

[^1]: Either onchain, or via a local use of the UPLC evaluator.
[^2]: `Q` is significantly more capable than `Term` is: for example, it can
    generate fresh names in a visible way, while `Term` can only do so 'behind
    the scenes'.
