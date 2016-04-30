module spec GHC.Prim where

embed GHC.Prim.Int#    as int
embed GHC.Prim.Word#   as int
embed GHC.Prim.Addr#   as int
embed GHC.Prim.Double# as real

measure addrLen
    :: GHC.Prim.Addr#
    -> GHC.Types.Int

+#
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int#
    -> {v: GHC.Prim.Int# | v = x + y}

-#
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int#
    -> {v: GHC.Prim.Int# | v = x - y}

==#
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int#
    -> {v:GHC.Prim.Int# | ((v = 1) <=> x = y)}

>=#
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int# 
    -> {v:GHC.Prim.Int# | ((v = 1) <=> x >= y)}

<=#
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int# 
    -> {v:GHC.Prim.Int# | ((v = 1) <=> x <= y)}

<# 
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int# 
    -> {v:GHC.Prim.Int# | ((v = 1) <=> x < y)}

>#
    :: x:GHC.Prim.Int#
    -> y:GHC.Prim.Int# 
    -> {v:GHC.Prim.Int# | ((v = 1) <=> x > y)}
