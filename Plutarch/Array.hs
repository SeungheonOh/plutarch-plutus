-- | @since 1.12.0
module Plutarch.Array (
  -- * Type
  PPullArray,

  -- * Functions

  -- ** Introduction
  piota,
  pgenerate,
  pfromArray,
  pfromList,

  -- ** Transformation

  -- *** Linear
  pmapArray,
  pimapArray,

  -- *** Affine
  ptakeArray,
  pdropArray,

  -- ** Combination
  pzipWithArray,
  pizipWithArray,

  -- ** Elimination

  -- *** Folds
  pfoldArray,
  prfoldArray,

  -- *** Conversions
  ppullArrayToList,
  ppullArrayToSOPList,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Array (
  PArray,
  pindexArray,
  plengthOfArray,
  plistToArray,
 )
import Plutarch.Builtin.Bool (pif)
import Plutarch.Builtin.Data (PBuiltinList (PNil), pconsBuiltin)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.Numeric (
  PAdditiveSemigroup (pscalePositive, (#+)),
  PMultiplicativeSemigroup (ppowPositive, (#*)),
  PNatural,
  (#-),
 )
import Plutarch.Internal.Ord (POrd ((#<=)), pmin)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PlutusType (PInner, pcon', pmatch'), pcon, pmatch)
import Plutarch.Internal.Subtype (pupcast)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.List (PList (PSCons, PSNil))

{- | A pull array, represented as its Boehm-Berrarducci encoding. Put another
way, a pull array is a function which can be \'materialized\' to produce the
elements of that array, in order.

Pull arrays give efficient linear transformations, in exchange for no ability
to index them without evaluation. Pull arrays are best used when you need to
perform a lot of transformations, with a fold or materialization at the end,
as they fuse away /all/ intermediate values. We achieve this by using
Boehm-Berrarducci encodings, which means that every pull array is essentially
a lambda onchain.

@since 1.12.0
-}
newtype PPullArray (a :: S -> Type) (s :: S)
  = PPullArray (forall (r :: S -> Type). Term s ((PNatural :--> (PInteger :--> a) :--> r) :--> r))

-- | @since 1.13.0
instance PlutusType (PPullArray a) where
  type PInner (PPullArray a) = PPullArray a
  pcon' (PPullArray t) = punsafeCoerce t
  pmatch' x f = f (PPullArray $ punsafeCoerce x)

-- | @since 1.12.0
instance PAdditiveSemigroup a => PAdditiveSemigroup (PPullArray a) where
  (#+) = pzipWithArray (plam (#+))
  pscalePositive arr p = pmapArray (plam $ \x -> pscalePositive x p) arr

-- | @since 1.12.0
instance PMultiplicativeSemigroup a => PMultiplicativeSemigroup (PPullArray a) where
  (#*) = pzipWithArray (plam (#*))
  ppowPositive arr p = pmapArray (plam $ \x -> ppowPositive x p) arr

{- | Given a length @n@, construct the pull array equivalent of @[0, 1, ... n -
1]@.

\(Theta(1)\) space and time complexity.

@since 1.12.0
-}
piota :: forall (s :: S). Term s PNatural -> Term s (PPullArray PNatural)
piota n = pcon $ PPullArray $ plam $ \k -> k # n # go
  where
    go :: forall (s' :: S). Term s' (PInteger :--> PNatural)
    go = phoistAcyclic $ plam $ \x -> punsafeCoerce x

{- | Given a length and a function from indexes to values, construct the pull
array of that length, each of whose indexes stores the value computed by that
function.

\(Theta(1)\) space and time complexity.

@since 1.12.0
-}
pgenerate ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PInteger :--> a) ->
  Term s (PPullArray a)
pgenerate len f = pcon $ PPullArray $ plam $ \k -> k # len # f

{- | Given a builtin array, construct the equivalent pull array.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pfromArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a) ->
  Term s (PPullArray a)
pfromArray arr = pcon $ PPullArray $ plam $ \k ->
  k # punsafeCoerce (plengthOfArray # arr) #$ pindexArray # arr

{- | Given a builtin list, construct the equivalent pull array. Uses
'plistToArray' internally.

\(\Theta(1)\) space complexity, \(\Theta(n)\) time complexity.

@since 1.12.0
-}
pfromList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBuiltinList a) ->
  Term s (PPullArray a)
pfromList ell = pfromArray (plistToArray # ell)

{- | Given a \'transformation function\' and a pull array, construct a new pull
array where each element of the argument array has been transformed without
moving it.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pmapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pmapArray f arr = pmatch arr $ \(PPullArray g) ->
  pcon $ PPullArray $ plam $ \k ->
    g # plam (\len h -> k # len #$ pcompose # f # h)
  where
    pcompose :: forall (s' :: S). Term s' ((a :--> b) :--> (PInteger :--> a) :--> PInteger :--> b)
    pcompose = phoistAcyclic $ plam $ \f1 f2 i -> f1 #$ f2 # i

{- | As 'pmapArray', but with an index-aware \'transformer function\'.

@since 1.12.0
-}
pimapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pimapArray f arr = pmatch arr $ \(PPullArray g) ->
  pcon $ PPullArray $ plam $ \k ->
    g # plam (\len h -> k # len #$ picompose # f # h)
  where
    picompose :: forall (s' :: S). Term s' ((PInteger :--> a :--> b) :--> (PInteger :--> a) :--> PInteger :--> b)
    picompose = phoistAcyclic $ plam $ \f1 f2 i -> f1 # i #$ f2 # i

{- | Given a size limit \(k\) and a pull array of length \(n\), construct a new
pull array that consists of the first \(\min \{k, n\}\) elements of the
argument pull array, at the same indexes.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
ptakeArray ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PPullArray a) ->
  Term s (PPullArray a)
ptakeArray lim arr = pmatch arr $ \(PPullArray g) ->
  pcon $ PPullArray $ plam $ \k ->
    g # plam (\len h -> k # pmin lim len # h)

{- | Given a desired number of discarded elements \(k\) and a pull array of length
\(n\), construct a new pull array that consists of all but the first \(min
\{k, n\}\) elements of the argument pull array, with indexes appropriately
offset.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pdropArray ::
  forall (a :: S -> Type) (s :: S).
  Term s PNatural ->
  Term s (PPullArray a) ->
  Term s (PPullArray a)
pdropArray dropped arr = pmatch arr $ \(PPullArray g) ->
  pcon $ PPullArray $ plam $ \k ->
    g # plam (\len h -> k # pdoz len dropped #$ go # h # dropped)
  where
    pdoz :: Term s PNatural -> Term s PNatural -> Term s PNatural
    pdoz x y = plet (pupcast @PInteger x #- pupcast y) $ \result ->
      punsafeCoerce $
        pif
          (result #<= (-1))
          0
          result
    go :: forall (s' :: S). Term s' ((PInteger :--> a) :--> PNatural :--> PInteger :--> a)
    go = phoistAcyclic $ plam $ \f x i -> f # (i #+ pupcast @PInteger x)

{- | Given a \'combining function\' and two pull arrays, produce a new pull
array whose length is the minimum of the lengths of the arguments, and whose
elements are applications of the \'combining function\' at the respective
indexes of the argument arrays.

\(\Theta(1)\) space and time complexity.

@since 1.12.0
-}
pzipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pzipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray k1) ->
  pmatch arr2 $ \(PPullArray k2) ->
    pcon $ PPullArray $ plam $ \k ->
      k1
        # plam
          ( \len1 h1 ->
              k2
                # plam
                  ( \len2 h2 ->
                      k # pmin len1 len2 #$ go # f # h1 # h2
                  )
          )
  where
    go ::
      forall (s' :: S).
      Term s' ((a :--> b :--> c) :--> (PInteger :--> a) :--> (PInteger :--> b) :--> PInteger :--> c)
    go = phoistAcyclic $ plam $ \combine ix1 ix2 i -> combine # (ix1 # i) #$ ix2 # i

{- | As 'pzipWithArray', but with an index-aware \'combining function\'.

@since 1.12.0
-}
pizipWithArray ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (PInteger :--> a :--> b :--> c) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b) ->
  Term s (PPullArray c)
pizipWithArray f arr1 arr2 = pmatch arr1 $ \(PPullArray k1) ->
  pmatch arr2 $ \(PPullArray k2) ->
    pcon $ PPullArray $ plam $ \k ->
      k1
        # plam
          ( \len1 h1 ->
              k2
                # plam
                  ( \len2 h2 ->
                      k # pmin len1 len2 #$ go # f # h1 # h2
                  )
          )
  where
    go ::
      forall (s' :: S).
      Term s' ((PInteger :--> a :--> b :--> c) :--> (PInteger :--> a) :--> (PInteger :--> b) :--> PInteger :--> c)
    go = phoistAcyclic $ plam $ \combine ix1 ix2 i -> combine # i # (ix1 # i) #$ ix2 # i

{- | Given a \'combining function\' and a starting value, reduce the argument
array by repeatedly combining elements with the starting value. This is a
left fold: thus, it will start at the lowest index and work its way upward.

Assuming \(\Theta(k)\) cost in space, and \(\Theta(\ell)\) cost in time, per
application of the \'combining function\', \(\Theta(kn)\) space complexity
and \(\Theta(k\ell)\) time complexity.

@since 1.13.0
-}
pfoldArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (b :--> a :--> b) ->
  Term s b ->
  Term s (PPullArray a) ->
  Term s b
pfoldArray f x arr = pmatch arr $ \(PPullArray k) ->
  k # plam (\len h -> go # f # h # pupcast len # x # 0)
  where
    go ::
      forall (s' :: S).
      Term s' ((b :--> a :--> b) :--> (PInteger :--> a) :--> PInteger :--> b :--> PInteger :--> b)
    go = phoistAcyclic $ pfix $ \self -> plam $ \combine get limit acc ix ->
      pif
        (ix #== limit)
        acc
        (self # combine # get # limit # (combine # acc #$ get # ix) # (ix + 1))

{- | As 'pfoldArray', but from the /highest/ index working /downward/.

@since 1.13.0
-}
prfoldArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (b :--> a :--> b) ->
  Term s b ->
  Term s (PPullArray a) ->
  Term s b
prfoldArray f x arr = pmatch arr $ \(PPullArray k) ->
  k # plam (\len h -> go # f # h # x # (pupcast len - 1))
  where
    go ::
      forall (s' :: S).
      Term s' ((b :--> a :--> b) :--> (PInteger :--> a) :--> b :--> PInteger :--> b)
    go = phoistAcyclic $ pfix $ \self -> plam $ \combine get acc ix ->
      pif
        (ix #== (-1))
        acc
        (self # combine # get # (combine # acc #$ get # ix) # (ix - 1))

{- | Convert a pull array to a builtin list. Prefer using this function to
either kind of fold, as it is faster.

If you want to construct a builtin /array/ instead, use this function
together with 'plistToArray'.

\(\Theta(n)\) space and time complexity.

@since 1.12.0
-}
ppullArrayToList ::
  forall (a :: S -> Type) (s :: S).
  PlutusType (PBuiltinList a) =>
  Term s (PPullArray a) ->
  Term s (PBuiltinList a)
ppullArrayToList arr = pmatch arr $ \(PPullArray k) ->
  k # plam (\len h -> go # h # pcon PNil # (pupcast len - 1))
  where
    go ::
      forall (s' :: S).
      Term s' ((PInteger :--> a) :--> PBuiltinList a :--> PInteger :--> PBuiltinList a)
    go = phoistAcyclic $ pfix $ \self -> plam $ \get acc ix ->
      pif
        (ix #== (-1))
        acc
        (self # get # (pconsBuiltin # (get # ix) # acc) # (ix - 1))

{- | Convert a pull array to a 'PList'. Prefer using this function to either
kind of fold, as it is faster.

\(\Theta(n)\) space and time complexity.

@since 1.12.0
-}
ppullArrayToSOPList ::
  forall (a :: S -> Type) (s :: S).
  Term s (PPullArray a) ->
  Term s (PList a)
ppullArrayToSOPList arr = pmatch arr $ \(PPullArray k) ->
  k # plam (\len h -> go # h # pcon PSNil # (pupcast len - 1))
  where
    go ::
      forall (s' :: S).
      Term s' ((PInteger :--> a) :--> PList a :--> PInteger :--> PList a)
    go = phoistAcyclic $ pfix $ \self -> plam $ \get acc ix ->
      pif
        (ix #== (-1))
        acc
        (self # get # (pcon . PSCons (get # ix) $ acc) # (ix - 1))
