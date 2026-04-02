# Encodings and representations onchain

We discuss the notion of _representations_ (how data 'looks') and _encodings_
(what exact mechanism we use to represent that 'look') when it comes to onchain
data, or specifically terms within UPLC. We will also examine what these choices
imply, and how to choose correctly for your application. Lastly, we will mention
how these concepts apply, and are used by, Plutarch.

## Prerequisites

Some familiarity with UPLC is helpful but not essential. In particular, it helps
to understand the UPLC performance model, or at least how it is measured. To
help with this, we recommend the MLabs blog article [_How Execution Budgeting Works for
On-Chain Scripts_][execution-budgeting].

## Representations

The only terms that can exist in a UPLC script are those whose types are part of
the (default) Plutus universe. These include, but are not limited to:

* Integers
* Byte strings
* UTF-8 strings
* Booleans
* BLS12-381 curve points
* Lists, pairs and arrays of terms in the default Plutus universe
* An SOP term (more on this later)
* `Data` (more on this later)

Each of these also comes with some _builtin operations_, which include ways to
introduce, operate on, and eliminate, these types. As UPLC is not typed, as long
as we are consistent, we're free to use whatever term we want for our scripts.
Our choice of terms for a given (intended) data type in our script is our
_representation_, and this ultimately determines several things:

* How much space our data takes up (in terms of onchain bytes);
* What things we can do with this data onchain;
* Whether we can pass the data to the script (as a datum for example);
* How efficiently we can operate on this data (in terms of onchain execution
  units).

As part of Plutarch's goal of giving as much control to script authors as
possible, we provide many tools to give precise representation control, while
maintaining as much type safety as possible.

[TODO: More, linkage]

## Encodings

[TODO: Explain builtin encoding]

[TODO: Explain `Data` encoding]

[TODO: Explain SOP encoding]

## What does this entail?

So, which encoding should you choose? While this will vary depending on what you
need your data to do, there are some basic rules:

* If your data needs to be passed in a datum or similar, `Data` encoding is your
  only choice.
* If you need a non-trivial ADT, especially if it's a sum type, SOP encoding is
  probably the best option.
* If you need a linear type with a fixed number of items (for example, a point
  in 3-dimensional space), prefer using SOP over a builtin list, as
  deconstructing lists for elements past the first is inefficient.
* Using a builtin pair as a representation is rarely worth it, as they are
  somewhat strange to construct, and aren't any faster to pattern match over
  than the equivalent SOP.

[TODO: More?]

To see how these can be applied, let's consider some choices made by Plutarch
itself. First, let's consider the `PNatural` data type, which is used to
represent non-negative integers. [TODO: Expand]

Second, let's consider [TODO: `Data` example]

Lastly, we examine [TODO: SOP example]

[TODO: Wrap]

[execution-budgeting]: https://www.mlabs.city/blog/how-execution-budgeting-works
