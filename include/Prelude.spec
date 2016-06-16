module spec Prelude where

import GHC.Base
import GHC.Int
import GHC.List
import GHC.Num
import GHC.Real
import GHC.Word

import Data.Foldable
import Data.Maybe
import GHC.Exts


GHC.Exts.D# :: x:_ -> {v:_ | v = x}

assume GHC.Base.. :: forall <p :: b -> c -> Prop, q :: a -> b -> Prop, r :: a -> c -> Prop>. 
                     {xcmp::a, wcmp::b<q xcmp> |- c<p wcmp> <: c<r xcmp>}
                     (ycmp:b -> c<p ycmp>)
                  -> (zcmp:a -> b<q zcmp>)
                  ->  xcmp:a -> c<r xcmp>
assume GHC.Integer.smallInteger :: x:GHC.Prim.Int#
                                -> { v:Prelude.Integer |
                                     v = (x :: int) }

assume GHC.Num.+ :: (GHC.Num.Num a) => x:a -> y:a -> {v:a | v = x + y }
assume GHC.Num.- :: (GHC.Num.Num a) => x:a -> y:a -> {v:a | v = x - y }





embed GHC.Types.Double as real
embed Prelude.Integer  as int

type GeInt N = {v: GHC.Types.Int | v >= N }
type LeInt N = {v: GHC.Types.Int | v <= N }
type Nat     = {v: GHC.Types.Int | v >= 0 }
type Even    = {v: GHC.Types.Int | (v mod 2) = 0 }
type Odd     = {v: GHC.Types.Int | (v mod 2) = 1 }
type BNat N  = {v: Nat           | v <= N }    
type TT      = {v: Bool          | Prop v}
type FF      = {v: Bool          | not (Prop v)}

predicate Max V X Y = if X > Y then V = X else V = Y
predicate Min V X Y = if X < Y then V = X else V = Y

type IncrListD a D = [a]<{\x y -> (x+D) <= y}>



//BOT: Do not delete EVER!

qualif Bot(v:@(0))    : (0 = 1)
qualif Bot(v:obj)     : (0 = 1)
qualif Bot(v:a)       : (0 = 1)
qualif Bot(v:bool)    : (0 = 1)
qualif Bot(v:int)     : (0 = 1)

qualif CmpZ(v:a)      : (v <  0)
qualif CmpZ(v:a)      : (v <= 0)
qualif CmpZ(v:a)      : (v >  0)
qualif CmpZ(v:a)      : (v >= 0)
qualif CmpZ(v:a)      : (v  = 0)
qualif CmpZ(v:a)      : (v != 0)

qualif Cmp(v:a, x:a)  : (v <  x)
qualif Cmp(v:a, x:a)  : (v <= x)
qualif Cmp(v:a, x:a)  : (v >  x)
qualif Cmp(v:a, x:a)  : (v >= x)
qualif Cmp(v:a, x:a)  : (v  = x)
qualif Cmp(v:a, x:a)  : (v != x)

// qualif CmpZ(v:a)        : v [ < ; <= ; > ; >= ; = ; != ] 0
// qualif Cmp(v:a,x:a)     : v [ < ; <= ; > ; >= ; = ; != ] x
// qualif Cmp(v:int,x:int) : v [ < ; <= ; > ; >= ; = ; != ] x

qualif One(v:int)     : v = 1
qualif True1(v:GHC.Types.Bool)   : (Prop(v))
qualif False1(v:GHC.Types.Bool)  : (~ Prop(v))


constant papp1 : func(1, [Pred @(0); @(0); bool])
qualif Papp(v:a,p:Pred a) : (papp1(p, v))

constant papp2 : func(4, [Pred @(0) @(1); @(2); @(3); bool])
qualif Papp2(v:a,x:b,p:Pred a b) : (papp2(p, v, x))

qualif Papp3(v:a,x:b, y:c, p:Pred a b c) : (papp3(p, v, x, y))
constant papp3 : func(6, [Pred @(0) @(1) @(2); @(3); @(4); @(5); bool])

// qualif Papp4(v:a,x:b, y:c, z:d, p:Pred a b c d) : papp4(p, v, x, y, z)
constant papp4 : func(8, [Pred @(0) @(1) @(2) @(6); @(3); @(4); @(5); @(7); bool])


constant Prop : func(0, [GHC.Types.Bool; bool])
