(* Wolfram Language Package *)

BeginPackage["MmaModelToC`", { "ProtectedSymbols`", "Format`","Stack`", "Experimental`","AsymptoticLinearization`"}]
(* Exported symbols added here with SymbolName::usage *)  
Print["preprivate"]
drib::usage="package top"
$mmaToCHome::usage="target location for generated files";
coeffDefines::usage="for splicing"
$runItExt::usage="for splicing"
$runItInv::usage="for splicing"
$runItOth::usage="for splicing"
$lagLeadLoc::usage="forsplicing"
(*
$runItExternalDefsLoc::usage="forsplicing"
$runItInvariantLocalDefsLoc::usage="forsplicing"
*)
$runoutFileStringLocalDefsLoc::usage="forsplicing"
ll:usage="for splicing"
opVarDefsSFA::usage="for splicing"
opLinVarDefsSFA::usage="for splicing"
opNLinVarDefsSFA::usage="for splicing"
opVarDefsDrvSFA::usage="for splicing"
opLinVarDefsDrvSFA::usage="for splicing"
opNLinVarDefsDrvSFA::usage="for splicing"
linSparseFunctionDerivativeAssignmentsA::usage="for splicing"
linSparseFunctionDerivativeAssignmentsIA::usage="for splicing"
linSparseFunctionDerivativeAssignmentsJA::usage="for splicing"
nlinSparseFunctionDerivativeAssignmentsA::usage="for splicing"
nlinSparseFunctionDerivativeAssignmentsIA::usage="for splicing"
nlinSparseFunctionDerivativeAssignmentsJA::usage="for splicing"
linSparseFunctionAssignmentsA::usage="for splicing"
linSparseFunctionAssignmentsIA::usage="for splicing"
linSparseFunctionAssignmentsJA::usage="for splicing"
nlinSparseFunctionAssignmentsA::usage="for splicing"
nlinSparseFunctionAssignmentsIA::usage="for splicing"
nlinSparseFunctionAssignmentsJA::usage="for splicing"
bLength::usage="for splicing"
outFileString::usage="for splicing"
stateVectorDefines::usage="forsplicing"
modelNumberOfEquations::usage="forsplicing"
aMat::usage="for splicing"
iaMat::usage="for splicing"
jaMat::usage="for splicing"
sparseFunctionDerivativeAssignmentsA::usage="forsplicing"
sparseFunctionDerivativeAssignmentsIA::usage="forsplicing"
sparseFunctionDerivativeAssignmentsJA::usage="forsplicing"
sparseFunctionAssignmentsA::usage="forsplicing"
sparseFunctionAssignmentsIA::usage="forsplicing"
sparseFunctionAssignmentsJA::usage="forsplicing"
modelData::usage="associates data with model"
modelExogenous::usage="associates data with model"
modelFunctionName::usage="associates data with model"
modelInfo::usage="associates data with model"
modelDataInfo::usage="associates data with model"
modelShockInfo::usage="associates data with model"
modelShocks::usage="associates data with model"
modelDefaultParameters::usage="associates data with model"
modelFpGuess::usage="associates data with model"
modelUpsilonEqns::usage="associates data with model"
spaceForTempVars::usage="for splicing"
modelColumns::usage="for splicing"
dataRows::usage="for splicing"
shocksRows::usage="for splicing"
numbExog::usage="for splicing"
upsilonMatA::usage="for splicing"
upsilonMatIA::usage="for splicing"
upsilonMatJA::usage="for splicing"
exogHMatA::usage="for splicing"
exogHMatIA::usage="for splicing"
exogHMatJA::usage="for splicing"
selectZMatA::usage="for splicing"
selectZMatIA::usage="for splicing"
selectZMatJA::usage="for splicing"
periodPointGuesserAssignments::usage="for splicing"
guessVector::usage="forsplicing"
timeOffset::usage="forsplicing"
numberOfParameters::usage="forsplicing"
vstr::usage="forsplicing"
dataCols::usage="for splicing"
shocksCols::usage="for splicing"
dstr::usage="for splicing"
allv::usage="for splicing"
allcoeffs::usage="for splicing"
defaultParams::usage="for splicing"
exogQ::usage="for splicing"
functionName::usage="for splicing"
(*
numberOfEquations
lags
leads
numberOfDataValues
*)
Begin["Private`"] (* Begin Private Context *) 


allNames[modelEquations_]:=Flatten[findVarsParams[modelEquations]]//.xx_[_]->xx
(*
allNames[modelEquations_]:=DeleteCases[
		Select[Union[Variables[(modelEquations//.funcSubs)//.
						getVarSubs]//.x_[_]->x],Length[#]==0&],FABS|FMAX|FMIN|myabsv|Log|modelShock];
*)
endog[modelEquations_]:=findVarsParams[modelEquations][[1]]
(*
endog[modelEquations_]:=Union[Select[DeleteCases[
		Select[Variables[(modelEquations//.funcSubs)//.getVarSubs],
				Length[#]==1&],FABS[_]|myabsv[_]|Log[_]|modelShock[_]]//.{x_[t]->x,x_[t+_]->x},Length[#]==0&]];
*)
$mmaMcFilesDir=FileNameJoin[Drop[FileNameSplit[FindFile["MmaModelToC`"]], -1]];
$lagLeadLoc=FileNameJoin[{$mmaMcFilesDir,"lagLead.h"}]
$runItExt=FileNameJoin[{$mmaMcFilesDir,"runItExternalDefs.h"}]
$runItInv=FileNameJoin[{$mmaMcFilesDir,"runItInvariantLocalDefs.h"}]
(*
$runItExternalDefsLoc=FileNameJoin[{$mmaMcFilesDir,"runItExternalDefs.h"}]
$runItInvariantLocalDefsLoc=FileNameJoin[{$mmaMcFilesDir,"runItInvariantLocalDefs.h"}]
*)
$runItOth=FileNameJoin[{$mmaMcFilesDir,"runItOther.h"}]
$runoutFileStringLocalDefsLoc=FileNameJoin[{$mmaMcFilesDir,"runoutFileStringLocalDefs.h"}]
Off[General::spell,General::spell1];
SetOptions[$Output,PageWidth->73];
Print["before reading Format and Optimize"]



Print["done reading Format and Optimize"]
Off[AssignFunction::undef]
(*funcSubs={Exp$->Exp,myabsv[x_]->x,mymax[x_,y_]->x,Sqrt$->Sqrt};*)
Print["doing assigns in mmaToC.m"]
drab="private"
Print[{"in private:",Context[drab],drab}]

funcSubs={Exp$->Exp,Sqrt$->Sqrt,FMIN[x_,y_]->x+y,FMAX[x_,y_]->x+y};
getVarSubs={Log[E^x_]->x,Power[x_,y_]->x y,Log[x_]->x};

twoNorm[dim_, x_] := 
  With[{lilvec = Take[x, -dim]}, 
    Inner[Times, lilvec, Conjugate[lilvec], Plus]/(dim*dim)]

ssSubstitutions=x_[_]->x


stateAssn[endog_]:=
MapIndexed[("#define " <> 
 ToString[#] <> 
 "(t)     (" <>"stateVector[t+2][" <>
 ToString[#2[[1]]-1] <> "])\n")&,endog];

linearStateAssn[modelEquations_]:=
With[{endog=endog[Union[modelEquations,Through[modelExogenous[modelEquations][t]]]],ll=lagsLeads[modelEquations]},
Join[MapIndexed[("#define " <> 
 ToString[#] <> 
 "(t)     (" <>"stateVector[(t-("<> ToString[ll[[1]]] <>"))*" <> 
 ToString[Length[endog]]<>"+" <>
 ToString[#2[[1]]-1] <> "])\n")&,endog],
MapIndexed[("#define " <> 
 "linPt$"<>ToString[#] <> 
 "(t)     (" <>"linearizationPoint[(t-("<> ToString[ll[[1]]] <>"))*" <> 
 ToString[Length[endog]]<>"+" <>
 ToString[#2[[1]]-1] <> "])\n")&,endog]]
];

modelDefaultParameterSubs[modelEquations_]:=Thread[coeffs[modelEquations]->modelDefaultParameters[modelEquations]];

coeffAssn[modelEquations_]:=
With[{coeffs=coeffs[modelEquations]},
MapIndexed[("#define " <> 
 ToString[#] <> " (parameters["<> ToString[#2[[1]]-1] <> "])\n")&,coeffs]];



lagsLeads[modelEquations_]:=
		Union[{0},Cases[modelEquations,x_[t+v_]->v,Infinity]];
coeffs[modelEquations_]:=With[{an=allNames[modelEquations],
		endg=endog[modelEquations]},Complement[an,endg,modelExogenous[modelEquations]]]

newEndog[modelEquations_]:=Union[Cases[modelEquations,x_[t]|x_[t+i_]->x,Infinity]]

justExog[modelEquations_]:=Union[
endog[modelUpsilonEqns[modelEquations]]]

justEndog[modelEquations_]:=Union[Complement[endog[modelEquations],justExog[modelEquations]]]

bothExogEndog[modelEquations_]:=Union[justExog[modelEquations],justEndog[modelEquations]]

allVars[modelEquations_]:=With[{ll=lagsLeads[modelEquations],
endog=Union[endog[modelEquations],modelExogenous[modelEquations]]},
		Flatten[Table[
		 Through[endog[t+i]],{i,Min[ll],Max[ll]}]]];

allExogVars[modelEquations_]:=With[{ll=
lagsLeads[modelUpsilonEqns[modelEquations]],
theExog=modelExogenous[modelEquations]},
If[ll[[1]]< -1 || ll[[-1]]>0,Print["Warning: expect AR(1) for upsilon: lags and/or leads out of range"]];
		Flatten[Table[
		 Through[theExog[t+i]],{i,Min[ll],Max[ll]}]]];

(*
drvs[modelEquations_]:=With[{av=allVars[modelEquations]},
((Print["doing",#];Function[x,D[x,#]&/@av]/@((modelEquations)/.funcSubs)))];
*)
drvs[modelEquations_List]:=Outer[(D[#1,#2])&,modelEquations/.funcSubs,allVars[modelEquations]]

(*differentiates exog model equations wrt full vector so columns correct*)
exogPrenewspdrv[modelEquations_]:=
With[{exg=modelExogenous[modelEquations]},
With[{allv=allVars[modelEquations],modeq=Through[exg[t]]},
With[{forOne=Function[x,
With[{relevant=Through[Select[exg,Not[FreeQ[x,#]]&][t]]},Print["just after defining relevant 1"];
(Print["mapping a row"];{Print["x=",x,"relevant=",relevant];D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modeq]]]

notExogPrenewspdrv[modelEquations_]:=
With[{exg=modelExogenous[modelEquations]},
With[{allv=allVars[modelEquations],modeq=Table[0,{Length[exg]}]},
With[{forOne=Function[x,
With[{relevant=Through[Select[exg,Not[FreeQ[x,#]]&][t]]},Print["just after defining relevant 2"];
(Print["mapping a row"];{Print["x=",x,"relevant=",relevant];D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modeq]]]

endogIn[modEq_,sys_List]:=With[{endg=endog[modEq]},Complement[endg,modelExogenous[sys]]]
exogIn[modEq_,sys_List]:=With[{endg=endog[modEq]},Intersection[endg,modelExogenous[sys]]]


prenewspdrv[modelEquations_]:=
With[{(*exg=modelExogenous[modelEquations]*)exg={}},
With[{allv=allVars[modelEquations]},
With[{endg=Select[allv,(And @@ (Function[x,FreeQ[#,x]] /@ exg))&]},
With[{forOne=Function[x,
With[{relevant=Select[endg,Not[FreeQ[x,#]]&]},Print["just after defining relevant 3"];
(Print["mapping a row, relevant=",relevant,"eqn=",x];{D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modelEquations]]]]

notPrenewspdrv[modelEquations_]:=
With[{endg=justEndog[modelEquations]},
With[{allv=allVars[modelUpsilonEqns[modelEquations]]},
With[{exg=Select[allv,(And @@ (Function[x,FreeQ[#,x]] /@ endg))&]},
With[{forOne=Function[x,
With[{relevant=Select[exg,Not[FreeQ[x,#]]&]},Print["just after defining relevant 4"];
(Print["mapping a row, relevant=",relevant,"eqn=",x];{D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modelEquations]]]]



(*this code differenctiates exog model equations but wrt
full vector so that sparse matrix has right column assignments*)
refPrenewspdrv[exogEqns_List,modelEquations_List]:=
With[{(*exg=modelExogenous[modelEquations]*)},
With[{allv=allExogVars[modelEquations],
modeq=exogEqns/.funcSubs},
With[{forOne=Function[x,
With[{relevant=Select[allv,Not[FreeQ[x,#]]&]},Print["just after defining relevant 5"];
(Print["mapping a row, relevant=",relevant,"eqn=",x];{D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modeq]]]


isExog[vbl_,modelEquations_List]:=
With[{longExog=justExog[modelEquations]},
Not[FreeQ[longExog,vbl]]];

forSplitSpdrvs[someEqns_,modelEquations_]:=
With[{res=Join[forSplitPrenewspdrv[someEqns,modelEquations],
resexg=exogPrenewspdrv[modelEquations]]
},
With[{lens=Length/@res},Append[Transpose[Partition[Flatten[res],2]],1+FoldList[Plus,0,lens]]]]

(*this code differenctiates exog model equations but wrt
full vector so that sparse matrix has right column assignments*)
forSplitPrenewspdrv[someEqns_List,modelEquations_List]:=
With[{exg=modelExogenous[modelEquations]},
With[{allv=allVars[modelEquations],
modeq=someEqns/.funcSubs},
With[{endg=Select[allv,(And @@ (Function[x,FreeQ[#,x]] /@ exg))&]},
With[{forOne=Function[x,
With[{relevant=Select[endg,Not[FreeQ[x,#]]&]},Print["just after defining relevant 6"];
(Print["mapping a row, relevant=",relevant,"eqn=",x];{D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modeq]]]]



refSpdrvs[exogEqns_,modelEquations_]:=
With[{res=refPrenewspdrv[exogEqns,modelEquations]},
With[{lens=Length/@res},Append[Transpose[Partition[Flatten[res],2]],1+FoldList[Plus,0,lens]]]]

spdrvs[modelEquations_]:=
With[{res=Join[prenewspdrv[modelEquations],
resexg=exogPrenewspdrv[modelEquations]]},
With[{lens=Length/@res},Append[Transpose[Partition[Flatten[res],2]],1+FoldList[Plus,0,lens]]]]


notSpdrvs[modelEquations_]:=
With[{res=Join[notPrenewspdrv[modelEquations],
resexg=notExogPrenewspdrv[modelEquations]]},
With[{lens=Length/@res},Append[Transpose[Partition[Flatten[res],2]],1+FoldList[Plus,0,lens]]]]


wrtExogPrenewspdrv[modelEquations_]:=
With[{exg=modelExogenous[modelEquations]},
With[{allv=allVars[modelEquations]},
With[{endg=Select[allv,(Or @@ (Function[x,!FreeQ[#,x]] /@ exg))&]},
With[{forOne=Function[x,
With[{relevant=Select[endg,Not[FreeQ[x,#]]&]},Print["just after defining relevant 7"];
(Print["mapping a row, relevant=",relevant,"eqn=",x];{D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modelEquations]]]]


justExogSpdrvs[modelEquations_]:=
With[{res=Join[resexg=wrtExogPrenewspdrv[modelEquations]]},
With[{lens=Length/@res},Append[Transpose[Partition[Flatten[res],2]],1+FoldList[Plus,0,lens]]]]

(*
spdrvs[modelEquations_]:=denseToSparseMat[drvs[modelEquations]];

*)
allSubs[modelEquations_]:=Thread[(endog[modelEquations])->Pi];
timeSubs={
(t-1)->tMaOne ,
(t-2)->tMaTwo ,
(t-3)->tMaThree ,
(t-4)->tMaFour ,
(t-5)->tMaFive ,
(t-6)->tMaSix ,
(t-7)->tMaSeven ,
(t-8)->tMaEight ,
(t-9)->tMaNine ,
(t-10)->tMaTen ,
(t-11)->tMaEleven ,
(t-12)->tMaTwelve ,
(t-13)->tMaThirteen ,
(t-14)->tMaFourteen ,
(t-15)->tMaFifteen ,
(t-16)->tMaSixteen ,
(t-17)->tMaSeventeen ,
(t-18)->tMaEighteen ,
(t-19)->tMaNineteen ,
(t-20)->tMaTwenty ,
(t-21)->tMaTwentyOne ,
(t-22)->tMaTwentyTwo ,
(t-23)->tMaTwentyThree ,
(t-24)->tMaTwentyFour ,
(t-25)->tMaTwentyFive ,
(t+1)->tPaOne ,
(t+2)->tPaTwo ,
(t+3)->tPaThree ,
(t+4)->tPaFour ,
(t+5)->tPaFive ,
(t+6)->tPaSix ,
(t+7)->tPaSeven ,
(t+8)->tPaEight ,
(t+9)->tPaNine ,
(t+10)->tPaTen ,
(t+11)->tPaEleven ,
(t+12)->tPaTwelve ,
(t+13)->tPaThirteen ,
(t+14)->tPaFourteen ,
(t+15)->tPaFifteen ,
(t+16)->tPaSixteen ,
(t+17)->tPaSeventeen ,
(t+18)->tPaEighteen ,
(t+19)->tPaNineteen ,
(t+20)->tPaTwenty ,
(t+21)->tPaTwentyOne ,
(t+22)->tPaTwentyTwo ,
(t+23)->tPaTwentyThree ,
(t+24)->tPaTwentyFour ,
(t+25)->tPaTwentyFive ,
t->0
}

spaceForTemp[modelEquations_]:=50000;

modelExogenous[modelEquations_]:=justExog[modelEquations];
modelFunctionName[_]:="function name omitted";
modelInfo[_]:="info omitted";
modelDataInfo[_]:="data info omitted";
modelData[_]:="data omitted";
modelShockInfo[_]:="shock info omitted";
modelShocks[_]:="shocks omitted";
modelDefaultParameters[_]:={};
modelFpGuess[_]:="fp guess omitted";
modelUpsilonEqns[_]:={};
modelUpsilonEqns[_]:={};
Unprotect[Derivative]
Derivative[1,0][FMAX][x_,y_]:=doRightSmaller[x,y]
Derivative[0,1][FMAX][x_,y_]:=(1-doRightSmaller[x,y])
Derivative[1,0][FMIN][x_,y_]:=(1-doRightSmaller[x,y])
Derivative[0,1][FMIN][x_,y_]:=doRightSmaller[x,y]
Derivative[1][FABS][x_]:=doSign[x]
Protect[Derivative]
(*
define a sparse matrix data structure
spMat[{val1,i1,j1},...{valn,in,jn}]
sumSameJ={spMat[{x_,_,b_},z___,{y_,_,b_}]->spMat[{x+y,$allI,b},z]};
sumSameI={spMat[{x_,a_,_},z___,{y_,a_,_}]->spMat[{x+y,a,$allJ},z]};
adds entries with same columns
SetAttributes[spMat,Orderless]

*)

sumSameIJ={spMat[zf___,{a_,b_,x_},zc___,{a_,b_,y_},zb___]:>spMat[zf,{a,b,x+y},zc,zb]};

multRow[{i_,j_,alph_},bmat_spMat]:=With[{arow=Cases[bmat,{j,k_,x_}]},
{i , #[[2]], alph #[[3]] } & /@arow]

vecToSpmat[vec_List]:=MapIndexed[{#2[[1]],1,#1}&,vec]

csrToSpmat[{a_List,ja_List,ia_List}]:=
Module[{},Print["csrToSpmat:starting"];
With[{prs=Partition[ia,2,1]},
With[{rws=Flatten[MapIndexed[Table[#2[[1]],{#1[[2]]-#[[1]]}]&, prs]]},
spMat @@Transpose[{rws,ja,a}]]]]

spMatToVec[sp_spMat,rws_Integer]:=
Module[{$toPop=Table[0,{rws}]},Print["in spMatToVec rws=",rws];
($toPop[[#[[1]]]]=#[[3]]) & /@ sp;
$toPop]

sparseAmuB[{a_,ja_,ia_},{b_,jb_,ib_}]:=
With[{spa=csrToSpmat[{a,ja,ia}],spb=csrToSpmat[{b,jb,ib}]},
(spMat @@((Join @@ DeleteCases[((Print["doing ",#];multRow[#,spb])& /@ 
spa),{}])))//.sumSameIJ]

trySeries[modelEquation_]:=
With[{allv=allVars[modelEquation]},
With[{linv=({#,ToExpression["linPt$" <>ToString[#]],1}& /@allv)},
With[{},
Normal[Series @@ Join[{(modelEquation)},linv]]]]];


avoidSeries[modelEquations_]:=
With[{drvmat=spdrvs[modelEquations],allv=allVars[modelEquations],
bth=bothExogEndog[modelEquations]},
With[{linv=(ToExpression["linPt$" <>ToString[#]]& /@allv)},
With[{forSub= Thread[allv->linv]},
With[{intcpt=Join[(modelEquations/.forSub),
Table[0,{Length[bth]-Length[modelEquations]}]],
prod=sparseAmuB[(drvmat/.forSub) , denseColToSparseMat[(allv-linv)]]},
intcpt+spMatToVec[prod,Length[bth]]]]]]

avoidSeries[modelEquations_,drvmat_]:=
With[{allv=allVars[modelEquations],
bth=bothExogEndog[modelEquations]},
With[{linv=(ToExpression["linPt$" <>ToString[#]]& /@allv)},
With[{forSub= Thread[allv->linv]},Print["avoidSeries:about to compute product"];
With[{intcpt=Join[(modelEquations/.forSub),
Table[0,{Length[bth]-Length[modelEquations]}]],
prod=sparseAmuB[(drvmat/.forSub) , denseColToSparseMat[(allv-linv)]]},
intcpt+spMatToVec[prod,Length[bth]]]]]]


oldAvoidSeries[modelEquations_]:=
With[{drvmat=drvs[modelEquations],allv=allVars[modelEquations]},
With[{linv=(ToExpression["linPt$" <>ToString[#]]& /@allv)},
With[{forSub= Thread[allv->linv]},
(modelEquations/.forSub) +((drvmat/.forSub) . (allv-linv))]]];
reallyLinearSubs={doRightSmaller[__]->1,FMAX[x_,y_]->x,FMIN[x_,y_]->y}(*always choose first arg as max or min*)

shftEqns[eqn_,maxLead_Integer]:=
With[{ll=lagsLeads[{eqn}]},
With[{needed=maxLead-ll[[-1]]},
Table[eqn/.t->t+i,{i,0,needed}]]]

bridge[modelEquations_List]:=
With[{ll=lagsLeads[modelEquations]},
With[{allShft=shftEqns[#,ll[[-1]]]& /@ modelEquations},
allShft]]



Print["sparseFunctionAssignments"];
genAIAJAAssn[modelEquations_List,
modelCSRMatrix:{theA_?VectorQ,theIA_?VectorQ,theJA_?VectorQ}]:=
With[{sfa=SFAAssign[modelEquations,theIA],
sfIA=CAssign[iaMat,theIA,
AssignOptimize->True,OptimizationSymbol -> okay,FormatType->OutputForm],
sfJA=CAssign[jaMat,theJA,
AssignOptimize->True,OptimizationSymbol -> okay,FormatType->OutputForm]},
With[{opVarDefs=genDefines[sfa]},
{sfa,opVarDefs,sfIA,sfJA}]]



doSplice[modelEquations_List,outFile_String]:= Module[{},
outFileString=ToString[outFile]; Print["spaceForTempVars probably not needed"];
spaceForTempVars=spaceForTemp[modelEquations];
Print["stateVectorDefines"]; stateVectorDefines=StringJoin @@
linearStateAssn[modelEquations]; Print["coeffDefines"];
coeffDefines=StringJoin @@ coeffAssn[modelEquations];
functionName=modelFunctionName[modelEquations];
exg=modelExogenous[modelEquations]; endg=endog[modelEquations];
allv=Union[exg,endg]; allcoeffs=coeffs[modelEquations];
numberOfParameters=Length[coeffs[modelEquations]];
ll=lagsLeads[modelEquations];
modelCreationInfo=modelInfo[modelEquations]; Print["modelMatrix"];
modelMatrix=denseColToSparseMat[Join[modelEquations(*/.funcSubs*),
Table[0,{Length[modelExogenous[modelEquations]]}]]]//.timeSubs;
forSeq= Sequence @@ ({#,ToExpression["linPt$" <>ToString[#]],1}&
/@allVars[modelEquations]); Print[modelMatrix//InputForm];Print["linModel"];
notsmodelSparseDrvs=spdrvs[modelEquations];
modelSparseDrvs=notsmodelSparseDrvs//.timeSubs;
complexLinModel=Join[(Print["starting
series"];avoidSeries[modelEquations,notsmodelSparseDrvs])[[Range[Length[modelEquations]]]],Table[0,{Length[modelExogenous[modelEquations]]}]];
linModel=Chop[Print["starting
simplify"];complexLinModel/.reallyLinearSubs];
(*linModel=Chop[Print["starting
simplify"];Simplify[complexLinModel/.reallyLinearSubs]];*)
Print["nlinPartModel"];Print["****",modelEquations,"****",linModel,"****"];
nlinPartModel=Join[Chop[Print["starting
simplify"];modelEquations-(linModel[[Range[Length[modelEquations]]]])],
Table[0,{Length[linModel]-Length[modelEquations]}]];Print["nlinpart",nlinPartModel];Print["?????",linModel[[2]],"?????",modelEquations[[2]],"????"];
(*nlinPartModel=Join[Chop[Print["starting
simplify"];Simplify[modelEquations-(linModel[[Range[Length[modelEquations]]]])]],
Table[0,{Length[linModel]-Length[modelEquations]}]];Print["nlinpart",nlinPartModel];Print["?????",linModel[[2]],"?????",modelEquations[[2]],"????"];*)
Print["computing linear part"];
linModelMatrix=denseColToSparseMat[linModel]//.timeSubs;
Print["computing non linear part"];
nlinModelMatrix=denseColToSparseMat[nlinPartModel]//.timeSubs;
Print["modelSparseDrvs"]; Print["differentiating linear part"];
linModelSparseDrvs=forSplitSpdrvs[linModel[[Range[Length[modelEquations]]]],
modelEquations]//.timeSubs; Print["differentiating non linear part"];
nlinModelSparseDrvs=forSplitSpdrvs[nlinPartModel[[Range[Length[modelEquations]]]],modelEquations]//.timeSubs;
modelNumberOfEquations=Length[Union[endg,modelExogenous[modelEquations]]];
numbExog=Length[modelExogenous[modelEquations]];
modelColumns=Abs[ll[[1]]]+ll[[-1]] +1;
Print["periodicPointGuesserAssignments"];
periodPointGuesserAssignments=
CAssign[guessVector[timeOffset],modelFpGuess[modelEquations],
AssignToArray->{guessVector}]; 
Print["sparseFunctionAssignments"];
{sparseFunctionAssignmentsA,opVarDefsSFA,
sparseFunctionAssignmentsIA,
sparseFunctionAssignmentsJA}=
genAIAJAAssn[modelEquations,modelMatrix];
Print["linSparseFunctionAssignments"];(*lin part*)
{linSparseFunctionAssignmentsA,opLinVarDefsSFA,
linSparseFunctionAssignmentsIA,
linSparseFunctionAssignmentsJA}=
genAIAJAAssn[modelEquations,linModelMatrix];
Print["nlinSparseFunctionAssignments"];(*nlpart*)
{nlinSparseFunctionAssignmentsA,opNLinVarDefsSFA,
nlinSparseFunctionAssignmentsIA,
nlinSparseFunctionAssignmentsJA}=
genAIAJAAssn[modelEquations,nlinModelMatrix];
Print["sparseDerivativeAssignments"];
{sparseFunctionDerivativeAssignmentsA,opVarDefsDrvSFA,
sparseFunctionDerivativeAssignmentsIA,
sparseFunctionDerivativeAssignmentsJA}=
genAIAJAAssn[modelEquations,modelMatrix];
Print["linSparseFunctionDerivativeAssignments"];(*lin part*)
{linSparseFunctionDerivativeAssignmentsA,opLinVarDefsDrvSFA,
linSparseFunctionDerivativeAssignmentsIA,
linSparseFunctionDerivativeAssignmentsJA}=
genAIAJAAssn[modelEquations,linModelMatrix];
Print["nlinSparseFunctionDerivativeAssignments"];(*nlpart*)
{nlinSparseFunctionDerivativeAssignmentsA,opNLinVarDefsDrvSFA,
nlinSparseFunctionDerivativeAssignmentsIA,
nlinSparseFunctionDerivativeAssignmentsJA}=
genAIAJAAssn[modelEquations,nlinModelMatrix];
Print["nlinSparseDerivativeAssignments"];
bLength=Length[modelSparseDrvs[[1]]];
upsilonMatrix=
If[modelUpsilonEqns[modelEquations]=={},denseToSparseMat[{{1}}],
refSpdrvs[
Through[modelExogenous[modelEquations][t]]/.Flatten[Solve[Thread[modelUpsilonEqns[modelEquations]==0]
,Through[modelExogenous[modelEquations][t]]]], modelEquations]];
upsilonMatA=CAssign[
aMat,upsilonMatrix[[1]],AssignOptimize->True,OptimizationSymbol ->
okay,FormatType->OutputForm];
upsilonMatIA= CAssign[
iaMat,upsilonMatrix[[3]],AssignOptimize->True,OptimizationSymbol ->
okay,FormatType->OutputForm];
upsilonMatJA= CAssign[
jaMat,upsilonMatrix[[2]],AssignOptimize->True,OptimizationSymbol ->
okay,FormatType->OutputForm];
exogHMatrix=
If[modelUpsilonEqns[modelEquations]=={},denseToSparseMat[{{1}}],
notSpdrvs[modelEquations]]//.timeSubs; exogHMatA=CAssign[
aMat,exogHMatrix[[1]],AssignOptimize->True,OptimizationSymbol -> okay,FormatType->OutputForm]; exogHMatIA=
CAssign[ iaMat,exogHMatrix[[3]],AssignOptimize->True,OptimizationSymbol
-> okay,FormatType->OutputForm];
exogHMatJA= CAssign[
jaMat,exogHMatrix[[2]],AssignOptimize->True,OptimizationSymbol -> okay,FormatType->OutputForm];
selectZMatrix={Table[1,{numbExog}],
Flatten[Position[bothExogEndog[modelEquations],#]& /@
justExog[modelEquations]], Range[numbExog+1]}; selectZMatA=CAssign[
aMat,selectZMatrix[[1]],AssignOptimize->True,OptimizationSymbol ->
okay,FormatType->OutputForm];
selectZMatIA= CAssign[
iaMat,selectZMatrix[[3]],AssignOptimize->True,OptimizationSymbol ->
okay,FormatType->OutputForm];
selectZMatJA= CAssign[
jaMat,selectZMatrix[[2]],AssignOptimize->True,OptimizationSymbol ->
okay,FormatType->OutputForm];
defaultParams=InputForm[N[Flatten[
modelDefaultParameters[modelEquations]]]]; numParams=Length[Flatten[
modelDefaultParameters[modelEquations]]];
fpGuessVec=modelFpGuess[modelEquations]; Print["data here"];
{dataRows,dataCols}=Dimensions[modelData[modelEquations]];Print[{dataRows,dataCols}];Print["huh",modelData[modelEquations]];
vstr=StringReplace[ToString[InputForm[N[Flatten[modelData[modelEquations]]]]],{"*^-"->"e-"}];
valsInfo=modelDataInfo[modelEquations]; Print["shocks"];
{shocksRows,shocksCols}=Dimensions[modelShocks[modelEquations]];
dvalsInfo=modelShocksInfo[modelEquations];
dstr=StringReplace[ToString[InputForm[N[Flatten[modelShocks[modelEquations]]]]],{"*^-"->"e-"}];
Print["splicing mmaToC.mc"];
Splice[$mmaMcFilesDir<>"/mmaToC.mc",outFile<>".c",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCDrv.mc"];
Splice[$mmaMcFilesDir<>"/mmaToCDrv.mc",outFile<>"Drv.c",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCSupport.mc"];
lngendg=bothExogEndog[modelEquations]; exg=justExog[modelEquations];
exogPos=Flatten[Position[lngendg,#]& /@ exg];
exogQ=Table[0,{Length[lngendg]}]; exogQ[[exogPos]]=1;
Splice[$mmaMcFilesDir<>"/mmaToCSupport.mc",outFile<>"Support.c",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCData.mc"];
Splice[$mmaMcFilesDir<>"/mmaToCData.mc",outFile<>"Data.c",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCShocks.mc"];
Splice[$mmaMcFilesDir<>"/mmaToCShocks.mc",outFile<>"Shocks.c",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing runIt.mc"]; Splice[$mmaMcFilesDir<>"/runIt.mc","run" <>
outFile<>".c",FormatType->OutputForm,PageWidth->Infinity];
Print["splicing mpiRunIt.mc"];
Splice[$mmaMcFilesDir<>"/mpiRunIt.mc","mpirun" <>
outFile<>".c",FormatType->OutputForm,PageWidth->Infinity];
Print["splicing makeFl.mc"];
Splice[$mmaMcFilesDir<>"/makeFl.mc","make"<>outFile,FormatType->OutputForm,PageWidth->Infinity];
Splice[$mmaMcFilesDir<>"/runItLocalDefs.mc","run"<>outFile<>"LocalDefs.h",FormatType->OutputForm,PageWidth->Infinity];
Splice[$mmaMcFilesDir<>"/mmaToCDataInclude.mc",outFile<>"DataForInclude.h",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Splice[$mmaMcFilesDir<>"/mmaToCShockInclude.mc",outFile<>"ShocksForInclude.h",FormatType->OutputForm,
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)]; ];

genDefines[theEqStr_String]:=
With[{opVarNames=StringCases[theEqStr,
RegularExpression["okay[0-9]+"]]},
(Union[ ("double "<># <>";\n")&/@opVarNames])<> "\n"]

SFAAssign[modelEquations_List,mMatrix_List]:=
With[{sReps=defArgsToIntsRepStrngs[endog[modelEquations]]},
With[{csn=CAssign[
aMat,mMatrix,AssignEnd->";\n",AssignOptimize->True,OptimizationSymbol -> okay,FormatType->OutputForm]},
StringReplace[StringJoin @@(csn[[1,1]]),sReps]]];

defArgsToIntsRepStrngs[varSymbs:{_Symbol...}]:=
With[{asStrs=ToString/@varSymbs},
With[{strSubs=
(RegularExpression[#<>"\\(([0-9]+)\\.\\)"]->#<>"($1)")&/@asStrs},
strSubs]]


doData[modelEquations_List,outFile_String]:=
Module[{},
functionName=modelFunctionName[modelEquations];
outFileString=ToString[outFile];
{dataRows,dataCols}=Dimensions[modelData[modelEquations]];;
vstr=StringReplace[ToString[InputForm[N[Flatten[modelData[modelEquations]]]]],{"*^-"->"e-"}];
valsInfo=modelDataInfo[modelEquations];
{shocksRows,shocksCols}=Dimensions[modelShocks[modelEquations]];;
dvalsInfo=modelShocksInfo[modelEquations];
dstr=StringReplace[ToString[InputForm[N[Flatten[modelShocks[modelEquations]]]]],{"*^-"->"e-"}];
Print["splicing mmaToCData.mc"];
Splice[$mmaMcFilesDir<>"/mmaToCData.mc",outFile<>"Data.c",FormatType->OutputForm,
	PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Splice[$mmaMcFilesDir<>"/mmaToCShockInclude.mc",outFile<>"ShocksForInclude.h",FormatType->OutputForm,
	PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Splice[$mmaMcFilesDir<>"/mmaToCDataInclude.mc",outFile<>"DataForInclude.h",FormatType->OutputForm,
	PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
];
Print["done reading MmaModelToC.m"]
(*

SetOptions[Experimental`OptimizeExpression,OptimizationSymbol -> aTmpVar]
SetOptions[$Output,PageWidth -> Infinity]

*)

End[] (* End Private Context *)

EndPackage[]