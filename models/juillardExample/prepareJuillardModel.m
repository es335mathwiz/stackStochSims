AppendTo[$Path,"../"];
(*
AppendTo[$Path,"/msu/home/m1gsa00/mathFiles/src/stack/"];
AppendTo[$Path,"/msu/res2/m1gsa00/dirDevConvergence/convergence/src/mathematica/
asymptotic/"];
*)


<<mmaToC.m
<<julliardModel.m
Needs["stack`"]
doSplice[julliardModel,"julliard"]
(*
to compile for debugging try
!gcc -c -g julliard.c
*)
