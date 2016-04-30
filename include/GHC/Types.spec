module spec GHC.Types where

import GHC.Prim


measure autolen :: forall a. a -> GHC.Types.Int

class measure len :: forall f a. f a -> GHC.Types.Int

instance measure len :: forall a. [a] -> GHC.Types.Int
len []     = 0
len (y:ys) = 1 + len ys

invariant {v: [a] | len(v) >= 0 }


embed GHC.Types.Int    as int
embed GHC.Types.Word   as int
embed GHC.Types.Double as real


embed Prop as bool
measure Prop :: GHC.Types.Bool -> Prop


type GeInt N = {v: GHC.Types.Int  | v >= N }
type LeInt N = {v: GHC.Types.Int  | v <= N }
type Nat     = {v: GHC.Types.Int  | v >= 0 }
type Even    = {v: GHC.Types.Int  | (v mod 2) = 0 }
type Odd     = {v: GHC.Types.Int  | (v mod 2) = 1 }
type BNat N  = {v: Nat            | v <= N }
type TT      = {v: GHC.Types.Bool | Prop v}
type FF      = {v: GHC.Types.Bool | not (Prop v)}

predicate Max V X Y = if X > Y then V = X else V = Y
predicate Min V X Y = if X < Y then V = X else V = Y


EQ
    :: {v:GHC.Types.Ordering | v = (cmp v) }

LT
    :: {v:GHC.Types.Ordering | v = (cmp v) }

GT
    :: {v:GHC.Types.Ordering | v = (cmp v) }

measure cmp :: GHC.Types.Ordering -> GHC.Types.Ordering
cmp GHC.Types.EQ = { v | v = GHC.Types.EQ }
cmp GHC.Types.LT = { v | v = GHC.Types.LT }
cmp GHC.Types.GT = { v | v = GHC.Types.GT }


True
    :: {v:GHC.Types.Bool | (Prop(v))}

False
    :: {v:GHC.Types.Bool | (~ (Prop(v)))}


isTrue#
    :: n:_
    -> {v:GHC.Types.Bool | ((n = 1) <=> (Prop(v)))}

I#
    :: x:GHC.Prim.Int#
    -> {v: GHC.Types.Int | v = (x :: int) }

W#
    :: w:_
    -> {v:GHC.Types.Word | v == w }
