------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Libraries (addLib) where

import Choco
import Const
import LamSyn
import Primitive
import Types
import Var
import Prelude hiding ((+), (-), (>), (<), (<=), (>=), (==))

import Control.Monad
import qualified Data.Map as M

addLib :: Lambda -> ChocoM Lambda
addLib cont = do
  -- floor
  a <- mkTmpVar "a" (toScheme FloatT)
  f <- mkTmpVar "f" (toScheme FloatT)
  let fun = Lfun [f] (Llet Strict (a, Lprim Pitof [Lprim Pftoi [Lvar f]]) (Lcond (Lprim (Pcompf Clt) [Lvar f, Llit (FloatC 0.0)])
        (Lcond (Lprim (Pcompf Ceq) [Lvar f, Lvar a]) (Lvar a)
          (Lprim Psubf [Lvar a, Llit (FloatC 1.0)]))
        (Lvar a)))

  let name = getPrimVar "floor"
  let f1 = Llet Strict (name, fun) 

  -- atan
  myatan_k <- mkGlobalVar "myatan_k" (toScheme (FunT [FloatT] FloatT))
  x <- mkTmpVar "x" (toScheme FloatT)
  z <- mkTmpVar "z" (toScheme FloatT)
  w <- mkTmpVar "w" (toScheme FloatT)

  let fun = Lfun [x] (Llet Strict (z, var x *. var x) (Llet Strict (w, var z *. var z) ((var z *. (int 0x3eaaaaaa +. (var w *. (int 0x3e124925 +. (var w *. (int 0x3dba2e6e +. (var w *. (int 0x3d886b35 +. (var w *. (int 0x3d4bda59 +. (var w *. int 0x3c8569d7))))))))))) +. (var w *. (int 0xbe4ccccd +. (var w *. (int 0xbde38e38 +. (var w *. (int 0xbd9d8795 +. (var w *. (int 0xbd6ef16b +. (var w *. int 0xbd15a221))))))))))))

  let f2 = f1 . Llet Strict (myatan_k, fun) 

  x  <- mkTmpVar "x" (toScheme FloatT)
  ix <- mkTmpVar "ix" (toScheme FloatT)
  nx <- mkTmpVar "nx" (toScheme FloatT)
  [z1, z2, z3, z4] <- replicateM 4 (mkTmpVar "z" (toScheme FloatT))
  let fun = Lfun [x] (Llet Strict (ix, fabs (var x)) (Lcond (var ix >= int 0x50800000) (Lcond (var x <. float 0.0) (int 0xbfc90fdb) (int 0x3fc90fdb)) (Lcond (var ix < int 0x31000000) (var x) (Lcond (var ix < int 0x3ee00000) (var x -. (var x *. (Lapp (Lvar myatan_k) [var x] True))) (Llet Variable (nx, fabs (var x)) (Lcond (var ix < int 0x3f300000) (Lseq (Lassign nx (((float 2.0 *. var nx) -. float 1.0) /. (float 2.0 +. var nx))) (Llet Strict (z1, int 0x3eed6338 -. (((var nx *. (Lapp (Lvar myatan_k) [var nx] True)) -. int 0x31ac3769) -. var nx)) (Lcond (var nx < int 0) (fneg (var z1)) (var z1)))) (Lcond (var ix < int 0x3f980000) (Lseq (Lassign nx ((var nx -. float 1.0) /. (var nx +. float 1.0))) (Llet Strict (z2, int 0x3f490fda -. (((var nx *. (Lapp (Lvar myatan_k) [var nx] True)) -. int 0x33222168) -. var nx)) (Lcond (var nx < int 0) (fneg (var z2)) (var z2)))) (Lcond (var ix < int 0x401c0000) (Lseq (Lassign nx ((var x -. float 1.5) /. (float 1.0 +. (float 1.5 *. var nx)))) (Llet Strict (z3, int 0x3f7b985e -. (((var nx *. (Lapp (Lvar myatan_k) [var nx] True)) -. int 0x33140fb4) -. var nx)) (Lcond (var nx < int 0) (fneg (var z3)) (var z3)))) (Lseq (Lassign nx (fneg (float 1.0 /. var x))) (Llet Strict (z4, int 0x3fc90fda -. (((var nx *. (Lapp (Lvar myatan_k) [var nx] True)) -. int 0x33a22168) -. var nx)) (Lcond (var nx < int 0) (fneg (var z)) (var z))))))))))))


  let name = getPrimVar "atan"
  let f3 = f2 . Llet Strict (name, fun) 

  j <- mkTmpVar "j" (toScheme UnknownT)
  i <- mkTmpVar "i" (toScheme UnknownT)
  l <- mkTmpVar "l" (toScheme UnknownT)
  k <- mkTmpVar "k" (toScheme UnknownT)
  let fun = Lfun [i] (Llet Strict (j, var i - (fabs (var i))) (Llet Strict (k, Lcond (var i > int 0) (var i) (int 0 - var i)) (Llet Strict (l, (var k + int 0x4b000000) -. int 0x4b000000) (Lcond (var k > int 0x00800000) (int 0) (var j - var l)))))
  let name = getPrimVar "float_of_int"
  let f4 = f3 . Llet Strict (name, fun)

  g <- mkTmpVar "g" (toScheme UnknownT)
  f <- mkTmpVar "f" (toScheme UnknownT)
  h <- mkTmpVar "h" (toScheme UnknownT)
  let fun = Lfun [f] (Llet Strict (g, fabs (var f)) (Lcond (var g > int 0x4b000000) (int 0) (Llet Strict (h, (var g +. int 0x4b000000) - int 0x4b000000) (Lcond (var f <= int 0) (int 0 - var h) (var h)))))
  let name = getPrimVar "int_of_float"
  let f5 = f4 . Llet Strict (name, fun) 

  v <- mkTmpVar "value" (toScheme UnknownT)
  t <- mkTmpVar "tmp" (toScheme UnknownT)
  i <- mkTmpVar "i" (toScheme IntT)
  a <- mkTmpVar "a" (toScheme UnknownT)
  let fun = Lfun [a] (Llet Variable (v, int 0) (Llet Variable (t, int (-1)) (Lseq (Lfor i (int 0) (int 3) (Lseq (Lassign t (int (-1))) (Lseq (Lwhile (var t < int 0) (Lassign t (Lprim Pget []))) (Lcond (var i == int 0) (Lassign v (var t)) (Lassign v ((var v << int 8) + var t)))))) (var v))))

  -- I/O
  read <- mkGlobalVar "read" (toScheme (FunT [UnitT] UnknownT))
  let name1 = getPrimVar "read_int"
  let name2 = getPrimVar "read_float"
  let f6 = f5 . Llet Strict (read, fun) . Llet Strict (name1, Lvar read) . Llet Strict (name2, Lvar read)

  -- sin/cos
  k_sin <- mkGlobalVar "k_sin" (toScheme (FunT [FloatT] FloatT))
  x     <- mkTmpVar "x" (toScheme UnknownT)
  y     <- mkTmpVar "y" (toScheme UnknownT)
  let fun = Lfun [x] (Llet Strict (y, var x *. var x) (var x *. (float 1.0 +. (var y *. (int 0xbe2aaaab +. (var y *. (int 0x3c088889 +. (var y *. (int 0xb9500d01 +. (var y *. (int 0x3638ef1b +. (var y *. (int 0xb2d72f34 +. (var y *. int 0x2f2ec9d3))))))))))))))
  let f7 = f6 . Llet Strict (k_sin, fun) 

  k_cos <- mkGlobalVar "k_cos" (toScheme (FunT [FloatT] FloatT))
  x     <- mkTmpVar "x" (toScheme UnknownT)
  y     <- mkTmpVar "y" (toScheme UnknownT)
  let fun = Lfun [x] (Llet Strict (y, var x *. var x) (float 1.0 +. (var y *. (int 0xbf000000 +. (var y *. (int 0x3d2aaaab +. (var y *. (int 0xbab60b61 +. (var y *. (int 0x37d00d01 +. (var y *. (int 0xb493f27c +. (var y *. (int 0x310f74f6 +. (var y *. int 0xad47d743)))))))))))))))
  let f8 = f7 . Llet Strict (k_cos, fun)

  reduction <- mkGlobalVar "reduction" (toScheme (FunT [FloatT] FloatT))
  x         <- mkTmpVar "x" (toScheme FloatT)
  t         <- mkTmpVar "t" (toScheme FloatT)
  r         <- mkTmpVar "r" (toScheme FloatT)
  let fun = Lfun [x] (Llet Variable (t, fabs (var x)) (Llet Variable (r, int 0) (Lseq (Lwhile (var t >. int 0x3f490fdb) (Lseq (Lassign t (var t -. int 0x3fc90fdb)) (Lassign r (Lcond (var r < int 3) (var r + int 1) (int 0))))) (Lprim PcreateTuple [var t, var r]))))
  let f9 = f8 . Llet Strict (reduction, fun)

  a <- mkTmpVar "a" (toScheme UnknownT)
  t <- mkTmpVar "t" (toScheme UnknownT)
  r <- mkTmpVar "r" (toScheme UnknownT)
  x <- mkTmpVar "x" (toScheme UnknownT)

  let fun = Lfun [a] (Llet Strict (x, Lapp (Lvar reduction) [var a] True) (Llet Strict (t, Lprim (PtupleRef 0) [Lvar x]) (Llet Strict (r, Lprim (PtupleRef 1) [Lvar x]) (Lcond (var r == int 0) (Lapp (Lvar k_cos) [var t] True) (Lcond (var r == int 1) (fneg (Lapp (Lvar k_sin) [var t] True)) (Lcond (var r == int 2) (fneg (Lapp (Lvar k_cos) [var t] True)) (Lapp (Lvar k_sin) [var t] True)))))))
  let name = getPrimVar "cos"
  let f10 = f9 . Llet Strict (name, fun) 

  a <- mkTmpVar "a" (toScheme UnknownT)
  t <- mkTmpVar "t" (toScheme UnknownT)
  r <- mkTmpVar "r" (toScheme UnknownT)
  x <- mkTmpVar "x" (toScheme UnknownT)
  tmp <- mkTmpVar "tmp" (toScheme UnknownT)
  let fun = Lfun [a] (Llet Strict (x, Lapp (Lvar reduction) [var a] True) (Llet Strict (t, Lprim (PtupleRef 0) [Lvar x]) (Llet Strict (r, Lprim (PtupleRef 1) [Lvar x]) (Llet Strict (tmp, Lcond (var r == int 0) (Lapp (Lvar k_sin) [var t] True) (Lcond (var r == int 1) (Lapp (Lvar k_cos) [var t] True) (Lcond (var r == int 2) (fneg (Lapp (Lvar k_sin) [var t] True)) (fneg (Lapp (Lvar k_cos) [var t] True))))) (Lcond (var a < int 0) (fneg (var tmp)) (var tmp))))))
  let name = getPrimVar "sin"
  let f11 = f10 . Llet Strict (name, fun) 

  return (f11 cont)

e1 + e2 = Lprim Paddi [e1, e2]
e1 - e2 = Lprim Psubi [e1, e2]
e1 +. e2 = Lprim Paddf [e1, e2]
e1 -. e2 = Lprim Psubf [e1, e2]
e1 *. e2 = Lprim Pmulf [e1, e2]
e1 /. e2 = Lprim Pdivf [e1, e2]
e1 <. e2 = Lprim (Pcompf Clt) [e1, e2]
e1 >. e2 = Lprim (Pcompf Cgt) [e1, e2]
e1 < e2  = Lprim (Pcompi Clt) [e1, e2]
e1 > e2  = Lprim (Pcompi Cgt) [e1, e2]
e1 <= e2 = Lprim (Pcompi Cle) [e1, e2]
e1 >= e2 = Lprim (Pcompi Cge) [e1, e2]
e1 == e2 = Lprim (Pcompi Ceq) [e1, e2]
e1 << e2 = Lprim Plsl [e1, e2]
int n   = Llit (IntC n)
float f = Llit (FloatC f)
var v   = Lvar v
fabs a  = Lprim Pabsf [a]
fneg a  = Lprim Pnegf [a]

getPrimVar :: String -> Var
getPrimVar name = 
  let (ty, id, _) = (fst primTable) M.! (mkName name) 
  in mkVar name id ty True
