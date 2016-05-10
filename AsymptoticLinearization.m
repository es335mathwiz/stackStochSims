(* Wolfram Language Package *)

BeginPackage["AsymptoticLinearization`",{"ProtectedSymbols`"}]
(* Exported symbols added here with SymbolName::usage *)  

(* 
Routines to Manipulate NonLinear Models for AIM Extended Path Algorithm

Gary S. Anderson 
    &   
Vi-Min Choong

August 13, 1996


*)



findVarsParams::usage="findVarsParams[func]"




steadyState::usage=
"     steadyState[modelFunction]
    Returns {List of functions of the parameters 
        with each function returning steady state values}(uses Solve)"



nSteadyState::usage=
"     nSteadyState[modelFunction]
    Returns {List of functions of the parameters 
        with each function returning steady state values}(uses FindRoot)"


makeFuncList::usage=
"     makeFuncList[modelFunction]
    Returns {List of functions of the parameters with each 
        function returning symbolic form steady state as a 
            function of parameters}"


ssLinearizeAndMakeHMat::usage=
"     ssLinearizeAndMakeHMat[modelFunction,selectedFixedPointFunction]
    Returns {Function providing the linearization 
            at the selected fixed point}"


maxLagLead::usage=
"maxLagLead[eqns]"



almodelFunc::usage=
"almodelFunc[func]"



Begin["Private`"]

findVarsParams[func_]:=
Module[{},
With[{fn=func //. t -> $timeMarker},
With[{vars=Union[(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker]]]]]),
(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker + y_]]]]])]},
With[{mathematicaFuncList={Equal,Sqrt,Exp,Log,Sin,Cos,Tan,Csc,Sec,Cot,ArcSin,
    ArcCos,ArcTan,ArcCsc,ArcSec,ArcCot,Sinh,Cosh,Tanh,Csch,Sech,
        Coth,ArcSinh,ArcCosh,ArcTanh,ArcCsch,ArcSech,ArcCoth,
            Abs,Pi,E,Exp,Re,Im,Plus,Times,Divide,Power,List,
                Sqrt,Derivative,D,Integrate,Head[func],
                    $timeMarker}},
With[{params=Complement[Union[
    Map[(Apply[Part , Prepend[#,fn]]) & , Position[fn,x_Symbol]]],
        mathematicaFuncList,vars]},{vars,params}]]]]]



(* Routine steadyState *)




(* Routine steadyState *)

steadyState[func_]:=
steadyState[func]=
Module[{},
With[{fn=func //. t -> $timeMarker},
With[{vars=Union[(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker]]]]]),
(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker + y_]]]]])]},
With[{mathematicaFuncList={Sqrt,Exp,Log,Sin,Cos,Tan,Csc,Sec,Cot,ArcSin,
    ArcCos,ArcTan,ArcCsc,ArcSec,ArcCot,Sinh,Cosh,Tanh,Csch,Sech,
        Coth,ArcSinh,ArcCosh,ArcTanh,ArcCsch,ArcSech,ArcCoth,
            Abs,Pi,E,Exp,Re,Im,Plus,Times,Divide,Power,List,
                Sqrt,Derivative,D,Integrate,Head[func],
                    $timeMarker}},
With[{params=Complement[Union[
    Map[(Apply[Part , Prepend[#,fn]]) & , Position[fn,x_Symbol]]],
        mathematicaFuncList,vars]},
    With[{eqns=(Map[#==0 & ,
         Level[fn,{2}]])/. {$timeMarker + _ -> $timeMarker}},
        With[{ss=(*Partition[Flatten[*)
            Solve[eqns,(Map[#[$timeMarker]& , vars])](*],
                Count[vars,x_Symbol]]*)},
{vars, Map[Apply[Function , {params, #}]& , 
    If[ss == {}, func,((Map[#[$timeMarker]&, vars]) /. ss)]]}]
]]]]]]




nSteadyState[func_]:=
Module[{},
With[{fn=func //. t -> $timeMarker},
With[{vars=Union[(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker]]]]]),
(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker + y_]]]]])]},
With[{mathematicaFuncList={Sqrt,Exp,Log,Sin,Cos,Tan,Csc,Sec,Cot,ArcSin,
    ArcCos,ArcTan,ArcCsc,ArcSec,ArcCot,Sinh,Cosh,Tanh,Csch,Sech,
        Coth,ArcSinh,ArcCosh,ArcTanh,ArcCsch,ArcSech,ArcCoth,
            Abs,Pi,E,Exp,Re,Im,Plus,Times,Divide,Power,List,
                Sqrt,Derivative,D,Integrate,Head[func],
                    $timeMarker}},
With[{params=Complement[Union[
    Map[(Apply[Part , Prepend[#,fn]]) & , Position[fn,x_Symbol]]],
        mathematicaFuncList,vars]},
    With[{eqns=(Map[#==0 & ,
         Level[fn,{2}]])/. {$timeMarker + _ -> $timeMarker},
        rootVars=Thread[{(Map[#[$timeMarker]& , vars]),1}]},
        With[{ss=
            Apply[FindRoot , Join[{eqns},rootVars]]},
{vars, Map[Apply[Function , {params, #}]& , 
    If[ss == {}, func,((Map[#[$timeMarker]&, vars]) /. ss)]]}]
]]]]]]




(* Routine makeFuncList *)

makeFuncList[func_]:=
makeFuncList[func]=
Module[{},
With[{fn=func //. t -> $timeMarker},
With[{vars=Union[(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker]]]]]),
(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker + y_]]]]])]},
With[{mathematicaFuncList={Sqrt,Exp,Log,Sin,Cos,Tan,Csc,Sec,Cot,ArcSin,
    ArcCos,ArcTan,ArcCsc,ArcSec,ArcCot,Sinh,Cosh,Tanh,Csch,Sech,
        Coth,ArcSinh,ArcCosh,ArcTanh,ArcCsch,ArcSech,ArcCoth,
            Abs,Pi,E,Exp,Re,Im,Plus,Times,Divide,Power,List,
                Sqrt,Derivative,D,Integrate,Head[func],
                    $timeMarker}},
With[{paramsSpec=Complement[Union[
    Map[(Apply[Part , Prepend[#,fn]]) & , Position[fn,x_Symbol]]],
        mathematicaFuncList,vars]},
With[{params=Map[Unique[ToString[#]]&,paramsSpec]},
    Apply[Function , {params, Map[(Apply[# , params])& , vars]}]
]]]]]]






maxLagLead[eqns_]:=
With[{ll=Join[Cases[eqns,_[t_]->0,Infinity],Cases[eqns,_[t+x_]->x,Infinity]]},
{-Min[ll],Max[ll]}]




almodelFunc[func_]:=
Module[{},
With[{fn=func //. t -> $timeMarker},
With[{vars=Union[(Map[Head,Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker]]]]]),
(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker + y_]]]]])]},
With[{mathematicaFuncList={Sqrt,Exp,Log,Sin,Cos,Tan,Csc,Sec,Cot,ArcSin,
    ArcCos,ArcTan,ArcCsc,ArcSec,ArcCot,Sinh,Cosh,Tanh,Csch,Sech,
        Coth,ArcSinh,ArcCosh,ArcTanh,ArcCsch,ArcSech,ArcCoth,
            Abs,Pi,E,Exp,Re,Im,Plus,Times,Divide,Power,List,
                Sqrt,Derivative,D,Integrate,Head[func],
                    $timeMarker}},
With[{params=Complement[Union[
    Map[(Apply[Part , Prepend[#,fn]]) & ,Position[fn,x_Symbol]]],
        mathematicaFuncList,vars]},
Apply[Function , {params, Apply[Function , {t,func[[1]]}]}]]]]]]





(* Routine ssLinearizeAndMakeHMat *)


ssLinearizeAndMakeHMat[func_,fixedPtFunc_]:=
(*ssLinearizeAndMakeHMat[func,fixedPtFunc]=*)
Module[{},
With[{fn=func //. t -> $timeMarker},
With[{vars=Union[(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker]]]]]),
(Map[Head , Union[Map[(Apply[Part , Prepend[#,fn]]) & ,
        Position[fn,x_[$timeMarker + y_]]]]])]},
With[{mathematicaFuncList={Sqrt,Exp,Log,Sin,Cos,Tan,Csc,Sec,Cot,ArcSin,
    ArcCos,ArcTan,ArcCsc,ArcSec,ArcCot,Sinh,Cosh,Tanh,Csch,Sech,
        Coth,ArcSinh,ArcCosh,ArcTanh,ArcCsch,ArcSech,ArcCoth,
            Abs,Pi,E,Exp,Re,Im,Plus,Times,Divide,Power,List,
                Sqrt,Derivative,D,Integrate,Head[func],
                    $timeMarker}},
With[{paramsSpec=Complement[Union[
    Map[(Apply[Part , Prepend[#,fn]]) & , Position[fn,x_Symbol]]],
        mathematicaFuncList,vars]},
With[{params=Map[Unique[ToString[#]]&,paramsSpec]},
With[{ssPrelim= fixedPtFunc /.
        Thread[Rule[paramsSpec,params]],
    fns=(func[[1]] /. 
        Flatten[{t -> tSpec,Thread[Rule[paramsSpec,params]]}])},
With[{sStates={MapThread[#1[$steadyStateT] -> #2 &,
        {vars,Apply[ssPrelim , ssPrelim[[1]]]}]}},
With[{ssSubs=sStates/.$steadyStateT->Blank[],
        tAppearing=Union[Map[#[[1]]& , Flatten[Map[(Cases[
            fns,(Apply[Alternatives , vars])[#],Infinity])& , 
                {tSpec, tSpec + _}]]]] - tSpec},
With[{tRange=Prepend[{Min[tAppearing],Max[tAppearing]},tVal]},
With[{vblsPresent=Flatten[Map[Table[#[tSpec+tVal],tRange]& , vars]]},
With[{},
With[{sslin= Part[Map[Function[ss,
Apply[Function , {params,
        CoefficientList[
         (Map[(Apply[Plus ,#])&, (Outer[(D[#1,#2]/.ss)(#2-(#2/.ss))&,
                fns,vblsPresent])]),
        Flatten[
        Transpose[Partition[vblsPresent,
                Max[tAppearing]-Min[tAppearing]+1]]]]}]],
        ssSubs],1]},
With[{maxLag=Min[tAppearing], 
    maxLead=Max[tAppearing], 
        neq=Length[fns],
            coeffLists=Part[sslin,2]},
With[{dpth=(maxLead-maxLag+1)neq},
With[{vals=((Map[(Union[Level[#,{dpth}]]&) , #]) &)[coeffLists],
        zeroRow={Table[0,{dpth}]}},
With[{further=
        MapThread[Function[{x,y},
        Map[{#,Select[Position[y,#],Length[#]==dpth&]}& , x]],
        {vals,coeffLists}]},
Apply[Function , {params,
ArrayFlatten@ Transpose @{
(Map[If[Head[#]==Integer,zeroRow,#,#]& ,
Map[Function[x,Apply[Plus, (Map[Take[FoldList[Plus,0,#],-1]&,
        (Map[(#[[2]]/.{1->0,2->#[[1]]})& , x])])]],further]])}}
        ]
]]]]]]]]]]]]]]]]]

(*
ArrayFlatten[Transpose[{{#}}]]&*)

End[]

EndPackage[]
Print["done reading AsymptoticLinearization"]

