(* Wolfram Language Package *)

BeginPackage["MmaModelToC`", { "ProtectedSymbols`", "Format`","Stack`", "Experimental`","AsymptoticLinearization`"}]
(* Exported symbols added here with SymbolName::usage *)  
Print["preprivate"]

myCAssign::usage="use ToString to eliminate OutputForm ColumnForm"



applyTemplates::usage="applyTemplates[eqns_?VectorQ,modName_String]"
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
okay::usage="used as matrix name in c code"
aMat::usage="for splicing"
iaMat::usage="for splicing"
jaMat::usage="for splicing"
tPaOne::usage="for splicing"
tMaOne::usage="for splicing"
guessVector::usage="forsplicing"
timeOffset::usage="forsplicing"
(*
mmaToCTemplate::usage="to gen c file"
drib::usage="package top"
$mmaToCHome::usage="target location for generated files";
coeffDefines::usage="for splicing"
$runItExt::usage="for splicing"
$runItInv::usage="for splicing"
$runItOth::usage="for splicing"
lagLeadLoc::usage="forsplicing"
(*
$runItExternalDefsLoc::usage="forsplicing"
$runItInvariantLocalDefsLoc::usage="forsplicing"
*)
$runoutFileStringLocalDefsLoc::usage="forsplicing"
ll::usage="for splicing"
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
modelCreationInfo::usage="for splicing"
modelMatrix::usage="for splicing"
(*
numberOfEquations
lags
leads
numberOfDataValues
*)
*)
Begin["Private`"] (* Begin Private Context *) 

myCAssign[lhs_:"",expr_?(!OptionQ[#]&),opts___?OptionQ]:=ToString[CAssign[lhs,expr,Flatten[{opts}]]]

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
mmaMcFilesDir=FileNameJoin[Drop[FileNameSplit[FindFile["MmaModelToC`"]], -1]];
lagLeadLoc=FileNameJoin[{mmaMcFilesDir,"lagLead.h"}]
$runItExt=FileNameJoin[{mmaMcFilesDir,"runItExternalDefs.h"}]
$runItInv=FileNameJoin[{mmaMcFilesDir,"runItInvariantLocalDefs.h"}]
$runItOth=FileNameJoin[{mmaMcFilesDir,"runItOther.h"}]
$runoutFileStringLocalDefsLoc=FileNameJoin[{mmaMcFilesDir,"runoutFileStringLocalDefs.h"}]
(*
$runItExternalDefsLoc=FileNameJoin[{mmaMcFilesDir,"runItExternalDefs.h"}]
$runItInvariantLocalDefsLoc=FileNameJoin[{mmaMcFilesDir,"runItInvariantLocalDefs.h"}]
*)
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
With[{relevant=Through[Select[exg,Not[FreeQ[x,#]]&][t]]},(*Print["just after defining relevant 1"];*)
((*Print["mapping a row"];*){(*Print["x=",x,"relevant=",relevant];*)D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modeq]]]

notExogPrenewspdrv[modelEquations_]:=
With[{exg=modelExogenous[modelEquations]},
With[{allv=allVars[modelEquations],modeq=Table[0,{Length[exg]}]},
With[{forOne=Function[x,
With[{relevant=Through[Select[exg,Not[FreeQ[x,#]]&][t]]},(*Print["just after defining relevant 2"];*)
((*Print["mapping a row"];*){(*Print["x=",x,"relevant=",relevant];*)D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modeq]]]

endogIn[modEq_,sys_List]:=With[{endg=endog[modEq]},Complement[endg,modelExogenous[sys]]]
exogIn[modEq_,sys_List]:=With[{endg=endog[modEq]},Intersection[endg,modelExogenous[sys]]]


prenewspdrv[modelEquations_]:=
With[{(*exg=modelExogenous[modelEquations]*)exg={}},
With[{allv=allVars[modelEquations]},
With[{endg=Select[allv,(And @@ (Function[x,FreeQ[#,x]] /@ exg))&]},
With[{forOne=Function[x,
With[{relevant=Select[endg,Not[FreeQ[x,#]]&]},(*Print["just after defining relevant 3"];*)
((*Print["mapping a row, relevant=",relevant,"eqn=",x];*){D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modelEquations]]]]

notPrenewspdrv[modelEquations_]:=
With[{endg=justEndog[modelEquations]},
With[{allv=allVars[modelUpsilonEqns[modelEquations]]},
With[{exg=Select[allv,(And @@ (Function[x,FreeQ[#,x]] /@ endg))&]},
With[{forOne=Function[x,
With[{relevant=Select[exg,Not[FreeQ[x,#]]&]},(*Print["just after defining relevant 4"];*)
((*Print["mapping a row, relevant=",relevant,"eqn=",x];*){D[x,#],Position[allv,#]})&/@relevant]]},
forOne/@modelEquations]]]]



(*this code differenctiates exog model equations but wrt
full vector so that sparse matrix has right column assignments*)
refPrenewspdrv[exogEqns_List,modelEquations_List]:=
With[{(*exg=modelExogenous[modelEquations]*)},
With[{allv=allExogVars[modelEquations],
modeq=exogEqns/.funcSubs},
With[{forOne=Function[x,
With[{relevant=Select[allv,Not[FreeQ[x,#]]&]},(*Print["just after defining relevant 5"];*)
((*Print["mapping a row, relevant=",relevant,"eqn=",x];*){D[x,#],Position[allv,#]})&/@relevant]]},
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
With[{relevant=Select[endg,Not[FreeQ[x,#]]&]},(*Print["just after defining relevant 6"];*)
((*Print["mapping a row, relevant=",relevant,"eqn=",x];*){D[x,#],Position[allv,#]})&/@relevant]]},
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
With[{relevant=Select[endg,Not[FreeQ[x,#]]&]},(*Print["just after defining relevant 7"];*)
((*Print["mapping a row, relevant=",relevant,"eqn=",x];*){D[x,#],Position[allv,#]})&/@relevant]]},
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
Module[{$toPop=Table[0,{rws}]},(*Print["in spMatToVec rws=",rws];*)
($toPop[[#[[1]]]]=#[[3]]) & /@ sp;
$toPop]

sparseAmuB[{a_,ja_,ia_},{b_,jb_,ib_}]:=
With[{spa=csrToSpmat[{a,ja,ia}],spb=csrToSpmat[{b,jb,ib}]},
(spMat @@((Join @@ DeleteCases[(((*Print["doing ",#];*)multRow[#,spb])& /@ 
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
With[{sfa=SFAAssign[modelEquations,theA],
sfIA=myCAssign[iaMat,theIA,AssignEnd->";\n",
AssignOptimize->True,OptimizationSymbol -> okay],
sfJA=myCAssign[jaMat,theJA,AssignEnd->";\n",
AssignOptimize->True,OptimizationSymbol -> okay]},
With[{opVarDefs=genDefines[sfa]},
{sfa,opVarDefs,sfIA,sfJA}]]



doSplice[modelEquations_List,outFile_String]:= 
Module[{outFileString,spaceForTempVars,functionName,stateVectorDefines,coeffDefines,exg,allv,
	numberOfParameters,ll,modelCreationInfo,modelMatrix,forSeq,endg,notsmodelSparseDrvs,modelSparseDrvs,complexLinModel,linModel,nlinPartModel,
	linModelMatrix,nlinModelMatrix,linModelSparseDrvs,nlinModelSparseDrvs,modelNumberOfEquations,numbExog,modelColumns,periodPointGuesserAssignments,
	sparseFunctionAssignmentsA,sparseFunctionAssignmentsIA,sparseFunctionAssignmentsJA,
	linSparseFunctionAssignmentsA,linSparseFunctionAssignmentsIA,linSparseFunctionAssignmentsJA,
	nlinSparseFunctionAssignmentsA,nlinSparseFunctionAssignmentsIA,nlinSparseFunctionAssignmentsJA,
	sparseFunctionDerivativeAssignmentsA,opVarDefsDrvSFA,
sparseFunctionDerivativeAssignmentsIA,
sparseFunctionDerivativeAssignmentsJA,linSparseFunctionDerivativeAssignmentsA,opLinVarDefsDrvSFA,
linSparseFunctionDerivativeAssignmentsIA,
linSparseFunctionDerivativeAssignmentsJA,
nlinSparseFunctionDerivativeAssignmentsA,opNLinVarDefsDrvSFA,
nlinSparseFunctionDerivativeAssignmentsIA,
nlinSparseFunctionDerivativeAssignmentsJA,
upsilonMatrix,upsilonMatA,upsilonMatIA,upsilonMatJA,
exogHMatrix,exogHMatA,exogHMatIA,exogHMatJA,
selectZMatrix,selectZMatA,selectZMatIA,selectZMatJA,
defaultParams,numParams,fpGuessVec,dataRows,dataCols,valsInfo,shocksRows,shocksCols,dvalsInfo,dstr,lndeng,exogPos,exogQ},
outFileString=ToString[outFile]; Print["spaceForTempVars probably not needed"];
spaceForTempVars=spaceForTemp[modelEquations];
Print["stateVectorDefines"]; 
stateVectorDefines=StringJoin @@
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
/@allVars[modelEquations]);(* Print[modelMatrix//InputForm];Print["linModel"];*)
notsmodelSparseDrvs=spdrvs[modelEquations];
modelSparseDrvs=notsmodelSparseDrvs//.timeSubs;
complexLinModel=Join[(Print["starting
series"];avoidSeries[modelEquations,notsmodelSparseDrvs])[[Range[Length[modelEquations]]]],Table[0,{Length[modelExogenous[modelEquations]]}]];
linModel=Chop[Print["starting
simplify"];complexLinModel/.reallyLinearSubs];
(*linModel=Chop[Print["starting
simplify"];Simplify[complexLinModel/.reallyLinearSubs]];*)
(*Print["nlinPartModel"];Print["****",modelEquations,"****",linModel,"****"];*)
nlinPartModel=Join[Chop[(*Print["starting simplify"];*)
	modelEquations-(linModel[[Range[Length[modelEquations]]]])],
Table[0,{Length[linModel]-Length[modelEquations]}]];(*Print["nlinpart",nlinPartModel];Print["?????",linModel[[2]],"?????",modelEquations[[2]],"????"];*)
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
myCAssign[guessVector[timeOffset],modelFpGuess[modelEquations],
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
upsilonMatA=myCAssign[
aMat,upsilonMatrix[[1]],AssignOptimize->True,OptimizationSymbol ->
okay];
upsilonMatIA= myCAssign[
iaMat,upsilonMatrix[[3]],AssignOptimize->True,OptimizationSymbol ->
okay];
upsilonMatJA= myCAssign[
jaMat,upsilonMatrix[[2]],AssignOptimize->True,OptimizationSymbol ->
okay];
exogHMatrix=
If[modelUpsilonEqns[modelEquations]=={},denseToSparseMat[{{1}}],
notSpdrvs[modelEquations]]//.timeSubs; exogHMatA=myCAssign[
aMat,exogHMatrix[[1]],AssignOptimize->True,OptimizationSymbol -> okay]; exogHMatIA=
myCAssign[ iaMat,exogHMatrix[[3]],AssignOptimize->True,OptimizationSymbol
-> okay];
exogHMatJA= myCAssign[
jaMat,exogHMatrix[[2]],AssignOptimize->True,OptimizationSymbol -> okay];
selectZMatrix={Table[1,{numbExog}],
Flatten[Position[bothExogEndog[modelEquations],#]& /@
justExog[modelEquations]], Range[numbExog+1]}; selectZMatA=myCAssign[
aMat,selectZMatrix[[1]],AssignOptimize->True,OptimizationSymbol ->
okay];
selectZMatIA= myCAssign[
iaMat,selectZMatrix[[3]],AssignOptimize->True,OptimizationSymbol ->
okay];
selectZMatJA= myCAssign[
jaMat,selectZMatrix[[2]],AssignOptimize->True,OptimizationSymbol ->
okay];
defaultParams=N[Flatten[
modelDefaultParameters[modelEquations]]];
numParams=Length[Flatten[
modelDefaultParameters[modelEquations]]];
fpGuessVec=modelFpGuess[modelEquations]; Print["data"];
{dataRows,dataCols}=Dimensions[modelData[modelEquations]];(*Print[{dataRows,dataCols}];Print["huh",modelData[modelEquations]];*)
vstr=StringReplace[ToString[InputForm[N[Flatten[modelData[modelEquations]]]]],{"*^-"->"e-"}];
valsInfo=modelDataInfo[modelEquations]; Print["shocks"];
{shocksRows,shocksCols}=Dimensions[modelShocks[modelEquations]];
dvalsInfo=modelShocksInfo[modelEquations];
dstr=StringReplace[ToString[InputForm[N[Flatten[modelShocks[modelEquations]]]]],{"*^-"->"e-"}];
Print["splicing mmaToC.mc"];
Splice[mmaMcFilesDir<>"/mmaToC.mc",outFile<>".c",
PageWidth->Infinity(*Max[100000,(11/10)*Rows*dataCols]*)];
Print["splicing mmaToCDrv.mc"];
Splice[mmaMcFilesDir<>"/mmaToCDrv.mc",outFile<>"Drv.c",
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCSupport.mc"];
lngendg=bothExogEndog[modelEquations]; exg=justExog[modelEquations];
exogPos=Flatten[Position[lngendg,#]& /@ exg];
exogQ=Table[0,{Length[lngendg]}]; exogQ[[exogPos]]=1;
Splice[mmaMcFilesDir<>"/mmaToCSupport.mc",outFile<>"Support.c",
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCData.mc"];
Splice[mmaMcFilesDir<>"/mmaToCData.mc",outFile<>"Data.c",
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing mmaToCShocks.mc"];
Splice[mmaMcFilesDir<>"/mmaToCShocks.mc",outFile<>"Shocks.c",
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Print["splicing runIt.mc"]; Splice[mmaMcFilesDir<>"/runIt.mc","run" <>
outFile<>".c",PageWidth->Infinity];
Print["splicing mpiRunIt.mc"];
Splice[mmaMcFilesDir<>"/mpiRunIt.mc","mpirun" <>
outFile<>".c",PageWidth->Infinity];
Print["splicing makeFl.mc"];
Splice[mmaMcFilesDir<>"/makeFl.mc","make"<>outFile,PageWidth->Infinity];
Splice[mmaMcFilesDir<>"/runItLocalDefs.mc","run"<>outFile<>"LocalDefs.h",PageWidth->Infinity];
Splice[mmaMcFilesDir<>"/mmaToCDataInclude.mc",outFile<>"DataForInclude.h",
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Splice[mmaMcFilesDir<>"/mmaToCShockInclude.mc",outFile<>"ShocksForInclude.h",
PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)]; ];

genDefines[theEqStr_String]:=
With[{opVarNames=StringCases[theEqStr,
RegularExpression["okay[0-9]+"]]},
(Union[ ("double "<># <>";\n")&/@opVarNames])<> "\n"]

SFAAssign[modelEquations_List,mMatrix_List]:=
With[{sReps=defArgsToIntsRepStrngs[endog[modelEquations]]},
With[{csn=myCAssign[
aMat,mMatrix,AssignEnd->";\n",AssignOptimize->True,OptimizationSymbol -> okay]},
StringReplace[(csn),sReps]]];

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
Splice[mmaMcFilesDir<>"/mmaToCData.mc",outFile<>"Data.c",
	PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Splice[mmaMcFilesDir<>"/mmaToCShockInclude.mc",outFile<>"ShocksForInclude.h",
	PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
Splice[mmaMcFilesDir<>"/mmaToCDataInclude.mc",outFile<>"DataForInclude.h",
	PageWidth->Infinity(*Max[100000,(11/10)*dataRows*dataCols]*)];
];
Print["done reading MmaModelToC.m"]
(*

SetOptions[Experimental`OptimizeExpression,OptimizationSymbol -> aTmpVar]
SetOptions[$Output,PageWidth -> Infinity]

*)
chkDelete[fName_String]:=If[FileExistsQ[fName],DeleteFile[fName]]

applyTemplates[eqns_?VectorQ,modName_String]:=
Module[{aList=makeModelDotCAList[eqns,modName]},
chkDelete[modName<>".c"];writeModelDotC[modName,aList];
chkDelete[modName<>"Drv.c"];writeModelDotCDrv[modName,aList];
chkDelete[modName<>"Makefile"];writeMakefile[modName,aList];
chkDelete["mpirun"<>modName<>".c"];writeMPIRun[modName,aList];
chkDelete[modName<>"DataForInclude.h"];writeDataInclude[modName,aList];
chkDelete[modName<>"Shocks.c"];writeShocks[modName,aList];
chkDelete[modName<>"ShocksForInclude.h"];writeShocksInclude[modName,aList];
chkDelete["run"<>modName<>"LocalDefs.h"];writeRunLocalDefs[modName,aList];
chkDelete[modName<>"Data.c"];writeData[modName,aList];
chkDelete["run"<>modName<>".c"];writeRun[modName,aList];
chkDelete[modName<>"Support.c"];writeCSupport[modName,aList];
]

part="/*Mathematica Creation Date `date`*/
/*`modelCreationInfo`*/
#include \"`lagLeadLoc`\"
#include <math.h>
`stateVectorDefines`
#define modelShock(n) (shockVec[n])
`coeffDefines`
void `functionName`(double *stateVector,double *parameters,
double * shockVec,
double * aMat,int * jaMat,int *iaMat,double * homotopyAlpha,double * linearizationPoint
)
{
int i;
double bMat[`modelNumberOfEquations`];
int ibMat[`modelNumberOfEquations`+1];
int jbMat[`modelNumberOfEquations`];
if(*homotopyAlpha>=1.0) {
`opVarDefsSFA`
`sparseFunctionAssignmentsA`

/*for(i=0;i<`modelNumberOfEquations`-`numbExog`;i++){aMat[i]=aMat[i]+shockVec[i];};*/


"

mmaToCTemplate="/*Mathematica Creation Date `date`*/
/*`modelCreationInfo`*/
#include \"`lagLeadLoc`\"
#include <math.h>
`stateVectorDefines`
#define modelShock(n) (shockVec[n])
`coeffDefines`
void `functionName`(double *stateVector,double *parameters,
double * shockVec,
double * aMat,int * jaMat,int *iaMat,double * homotopyAlpha,double * linearizationPoint
)
{
int i;
double bMat[`modelNumberOfEquations`];
int ibMat[`modelNumberOfEquations`+1];
int jbMat[`modelNumberOfEquations`];
if(*homotopyAlpha>=1.0) {
`opVarDefsSFA`
`sparseFunctionAssignmentsA`
/*for(i=0;i<`modelNumberOfEquations`-`numbExog`;i++){aMat[i]=aMat[i]+shockVec[i];};*/
`sparseFunctionAssignmentsIA`
`sparseFunctionAssignmentsJA`
} else {
`opLinVarDefsSFA`
`linSparseFunctionAssignmentsA`
/*for(i=0;i<`modelNumberOfEquations`-`numbExog`;i++){aMat[i]=aMat[i]+shockVec[i];};*/
`linSparseFunctionAssignmentsIA`
`linSparseFunctionAssignmentsJA`
if(*homotopyAlpha>0.0) {
`opNLinVarDefsSFA`
`nlinSparseFunctionAssignmentsA`
`nlinSparseFunctionAssignmentsIA`
`nlinSparseFunctionAssignmentsJA`
for(i=0;i<`modelNumberOfEquations`;i++){aMat[i]=aMat[i]+(*homotopyAlpha*bMat[i]);};
}
}
}
"


makeModelDotCAList[modelEquations_List,outRoot_String]:=
Module[{lags,leads,ll,notsmodelSparseDrvs,modelSparseDrvs,complexLinModel,linModel,notsrvs,rvs},
notsmodelSparseDrvs=spdrvs[modelEquations];
modelSparseDrvs=notsmodelSparseDrvs//.timeSubs;
notsrvs=spdrvs[modelEquations];
rvs=notsmodelSparseDrvs//.timeSubs;
complexLinModel=Join[
	(avoidSeries[modelEquations,notsmodelSparseDrvs])[[Range[Length[modelEquations]]]],
	Table[0,{Length[modelExogenous[modelEquations]]}]];
linModel=Chop[complexLinModel/.reallyLinearSubs];
With[{nlinPartModel=Join[Chop[
	modelEquations-(linModel[[Range[Length[modelEquations]]]])],
Table[0,{Length[linModel]-Length[modelEquations]}]]},
With[{modelMatrix=denseColToSparseMat[Join[modelEquations(*/.funcSubs*),
Table[0,{Length[modelExogenous[modelEquations]]}]]]//.timeSubs,
linModelMatrix=denseColToSparseMat[linModel]//.timeSubs,
nlinModelMatrix=denseColToSparseMat[nlinPartModel]//.timeSubs},
Module[{modelColumns,theNamesArray,theParamNamesArray,
sparseFunctionAssignmentsA,opVarDefsSFA,
sparseFunctionAssignmentsIA,
sparseFunctionAssignmentsJA,
linSparseFunctionAssignmentsA,opLinVarDefsSFA,
linSparseFunctionAssignmentsIA,
linSparseFunctionAssignmentsJA,
nlinSparseFunctionAssignmentsA,opNLinVarDefsSFA,
nlinSparseFunctionAssignmentsIA,
nlinSparseFunctionAssignmentsJA,
sparseFunctionDerivativeAssignmentsA,opVarDefsDrvSFA,
sparseFunctionDerivativeAssignmentsIA,
sparseFunctionDerivativeAssignmentsJA,
linSparseFunctionDerivativeAssignmentsA,opLinVarDefsDrvSFA,
linSparseFunctionDerivativeAssignmentsIA,
linSparseFunctionDerivativeAssignmentsJA,
nlinSparseFunctionDerivativeAssignmentsA,opNLinVarDefsDrvSFA,
nlinSparseFunctionDerivativeAssignmentsIA,
nlinSparseFunctionDerivativeAssignmentsJA},
lngendg=bothExogEndog[modelEquations];
exg=justExog[modelEquations];
exogPos=Flatten[Position[lngendg,#]& /@ exg];
exogQ=Table[0,{Length[lngendg]}];
exogQ[[exogPos]]=1;
theNamesArray=Union[("\""<>ToString[Head[#]]<>"\"")& /@ allVars[modelEquations]];
theParamNamesArray=("\""<>ToString[#]<>"\"")& /@ coeffs[modelEquations];
numberOfParameters=Length[coeffs[modelEquations]];
{dataRows,dataCols}=Dimensions[modelData[modelEquations]];
{shocksRows,shocksCols}=Dimensions[modelShocks[modelEquations]];
{sparseFunctionAssignmentsA,opVarDefsSFA,
sparseFunctionAssignmentsIA,
sparseFunctionAssignmentsJA}=
genAIAJAAssn[modelEquations,modelMatrix];
{linSparseFunctionAssignmentsA,opLinVarDefsSFA,
linSparseFunctionAssignmentsIA,
linSparseFunctionAssignmentsJA}=
genAIAJAAssn[modelEquations,linModelMatrix];
{nlinSparseFunctionAssignmentsA,opNLinVarDefsSFA,
nlinSparseFunctionAssignmentsIA,
nlinSparseFunctionAssignmentsJA}=
genAIAJAAssn[modelEquations,nlinModelMatrix];
{sparseFunctionDerivativeAssignmentsA,opVarDefsDrvSFA,
sparseFunctionDerivativeAssignmentsIA,
sparseFunctionDerivativeAssignmentsJA}=
genAIAJAAssn[modelEquations,modelMatrix];
{linSparseFunctionDerivativeAssignmentsA,opLinVarDefsDrvSFA,
linSparseFunctionDerivativeAssignmentsIA,
linSparseFunctionDerivativeAssignmentsJA}=
genAIAJAAssn[modelEquations,linModelMatrix];
{nlinSparseFunctionDerivativeAssignmentsA,opNLinVarDefsDrvSFA,
nlinSparseFunctionDerivativeAssignmentsIA,
nlinSparseFunctionDerivativeAssignmentsJA}=
genAIAJAAssn[modelEquations,nlinModelMatrix];
ll=lagsLeads[modelEquations];
lags=-ll[[1]];
leads=ll[[-1]];
modelColumns=Abs[ll[[1]]]+ll[[-1]] +1;
defaultParams=N[Flatten[
modelDefaultParameters[modelEquations]]];
numParams=Length[Flatten[
modelDefaultParameters[modelEquations]]];
endg=endog[modelEquations];
allv=Union[exg,endg];
allcoeffs=coeffs[modelEquations];
theAllv=ToString /@ allv;
theAllCoeffs=ToString /@ allcoeffs;
runItExt=FileNameJoin[{mmaMcFilesDir,"runItExternalDefs.h"}];
runItInv=FileNameJoin[{mmaMcFilesDir,"runItInvariantLocalDefs.h"}];
runItOth=FileNameJoin[{mmaMcFilesDir,"runItOther.h"}];
spaceForTempVars=spaceForTemp[modelEquations];
periodPointGuesserAssignments=
myCAssign[guessVector[timeOffset],modelFpGuess[modelEquations],
AssignToArray->{guessVector}];
upsilonMatrix=
If[modelUpsilonEqns[modelEquations]=={},denseToSparseMat[{{1}}],
refSpdrvs[
Through[modelExogenous[modelEquations][t]]/.Flatten[Solve[Thread[modelUpsilonEqns[modelEquations]==0]
,Through[modelExogenous[modelEquations][t]]]], modelEquations]];
upsilonMatA=myCAssign[
aMat,upsilonMatrix[[1]],AssignOptimize->True,OptimizationSymbol ->
okay];
upsilonMatIA= myCAssign[
iaMat,upsilonMatrix[[3]],AssignOptimize->True,OptimizationSymbol ->
okay];
upsilonMatJA= myCAssign[
jaMat,upsilonMatrix[[2]],AssignOptimize->True,OptimizationSymbol ->
okay];
exogHMatrix=
If[modelUpsilonEqns[modelEquations]=={},denseToSparseMat[{{1}}],
notSpdrvs[modelEquations]]//.timeSubs; exogHMatA=myCAssign[
aMat,exogHMatrix[[1]],AssignOptimize->True,OptimizationSymbol -> okay]; exogHMatIA=
myCAssign[ iaMat,exogHMatrix[[3]],AssignOptimize->True,OptimizationSymbol
-> okay];
exogHMatJA= myCAssign[
jaMat,exogHMatrix[[2]],AssignOptimize->True,OptimizationSymbol -> okay];
numbExog=Length[modelExogenous[modelEquations]];
selectZMatrix={Table[1,{numbExog}],
Flatten[Position[bothExogEndog[modelEquations],#]& /@
justExog[modelEquations]], Range[numbExog+1]}; selectZMatA=myCAssign[
aMat,selectZMatrix[[1]],AssignOptimize->True,OptimizationSymbol ->
okay];
selectZMatIA= myCAssign[
iaMat,selectZMatrix[[3]],AssignOptimize->True,OptimizationSymbol ->
okay];
selectZMatJA= myCAssign[
jaMat,selectZMatrix[[2]],AssignOptimize->True,OptimizationSymbol ->
okay];
vstr=StringReplace[ToString[InputForm[N[Flatten[modelData[modelEquations]]]]],{"*^-"->"e-"}];
dvalsInfo=modelShocksInfo[modelEquations];
dstr=StringReplace[ToString[InputForm[N[Flatten[modelShocks[modelEquations]]]]],{"*^-"->"e-"}];
With[{neq=Length[Union[endog[modelEquations],modelExogenous[modelEquations]]]},
<|"date"->Date[],
"modelCreationInfo"->modelInfo[modelEquations],
"lagLeadLoc"->FileNameJoin[{mmaMcFilesDir,"lagLead.h"}],
"stateVectorDefines"->StringJoin @@ linearStateAssn[modelEquations],
"coeffDefines"->StringJoin @@ coeffAssn[modelEquations],
"functionName"->modelFunctionName[modelEquations],
"modelNumberOfEquations"->neq,
"opVarDefsSFA"->opVarDefsSFA,
"numbExog"->numbExog,
"sparseFunctionAssignmentsA"->sparseFunctionAssignmentsA,
"modelNumberOfEquations-numbExog"->neq-numbExog,
"sparseFunctionAssignmentsIA"->sparseFunctionAssignmentsIA,
"sparseFunctionAssignmentsJA"->sparseFunctionAssignmentsJA,
"opLinVarDefsSFA"->opLinVarDefsSFA,
"linSparseFunctionAssignmentsA"->linSparseFunctionAssignmentsA,
"linSparseFunctionAssignmentsIA"->linSparseFunctionAssignmentsIA,
"linSparseFunctionAssignmentsJA"->linSparseFunctionAssignmentsJA,
"opNLinVarDefsSFA"->opNLinVarDefsSFA,
"nlinSparseFunctionAssignmentsA"->nlinSparseFunctionAssignmentsA,
"nlinSparseFunctionAssignmentsIA"->nlinSparseFunctionAssignmentsIA,
"nlinSparseFunctionAssignmentsJA"->nlinSparseFunctionAssignmentsJA,
"bLength"->Length[modelSparseDrvs[[1]]],
"opVarDefsDrvSFA"->opVarDefsDrvSFA,
"sparseFunctionDerivativeAssignmentsA"->sparseFunctionDerivativeAssignmentsA,
"sparseFunctionDerivativeAssignmentsIA"->sparseFunctionDerivativeAssignmentsIA,
"sparseFunctionDerivativeAssignmentsJA"->sparseFunctionDerivativeAssignmentsJA,
"opLinVarDefsDrvSFA"->opLinVarDefsDrvSFA,
"linSparseFunctionDerivativeAssignmentsA"->linSparseFunctionDerivativeAssignmentsA,
"linSparseFunctionDerivativeAssignmentsIA"->linSparseFunctionDerivativeAssignmentsIA,
"linSparseFunctionDerivativeAssignmentsJA"->linSparseFunctionDerivativeAssignmentsJA,
"opNLinVarDefsDrvSFA"->opNLinVarDefsDrvSFA,
"nlinSparseFunctionDerivativeAssignmentsA"->nlinSparseFunctionDerivativeAssignmentsA,
"nlinSparseFunctionDerivativeAssignmentsIA"->nlinSparseFunctionDerivativeAssignmentsIA,
"nlinSparseFunctionDerivativeAssignmentsJA"->nlinSparseFunctionDerivativeAssignmentsJA,
"outFile"->outRoot,
"modelColumns"->modelColumns,
"lags"->lags,
"leads"->leads,
"theNamesArray"->theNamesArray,
"theParamNamesArray"->theParamNamesArray,
"numParams"->numParams,
"defaultParams"->defaultParams,
"exogQ"->exogQ,
"theAllv"->theAllv,
"theAllCoeffs"->theAllCoeffs,
"runItExt"->runItExt,
"runItInv"->runItInv,
"runItOth"->runItOth,
"numberOfParameters"->numberOfParameters,
"shocksRows"->shocksRows,
"shocksCols"->shocksCols,
"dataRows"->dataRows,
"dataCols"->dataCols,
"spaceForTempVars"->spaceForTempVars,
"periodPointGuesserAssignments"->periodPointGuesserAssignments,
"upsilonMatA"->upsilonMatA,
"upsilonMatIA"->upsilonMatIA,
"upsilonMatJA"->upsilonMatJA,
"exogHMatA"->exogHMatA,
"exogHMatIA"->exogHMatIA,
"exogHMatJA"->exogHMatJA,
"selectZMatA"->selectZMatA,
"selectZMatIA"->selectZMatIA,
"selectZMatJA"->selectZMatJA,
"vstr"->vstr,
"dvalsInfo"->"dvalsInfo",
"dstr"->dstr
|>]]]]]



writeModelDotC[outFile_String,aList_Association]:=
With[{theStr=TemplateApply[mmaToCTemplate,aList]},
WriteString[outFile<>".c",
	theStr]]


writeModelDotCDrv[outFile_String,aList_Association]:=
WriteString[outFile<>"Drv.c",
	TemplateApply[mmaToCDrvTemplate,aList]]

mmaToCDrvTemplate="

/*Mathematica Creation Date`date`*/
/*`modelCreationInfo`*/
#include \"`lagLeadLoc`\"
#include <math.h>
`stateVectorDefines`
#define modelShock(n) (shockVec[n])
`coeffDefines`
  







void `functionName`Derivative(double *stateVector,double *parameters,
double * shockVec,
double * aMat,int * jaMat,int *iaMat,double * homotopyAlpha,double * linearizationPoint
)
{int i;
double bMat[`bLength`];
int ibMat[`modelNumberOfEquations`+1];
int jbMat[`bLength`];
double cMat[`bLength`];
int icMat[`modelNumberOfEquations`+1];
int jcMat[`bLength`];
int aOne=1;int ierr;int maxNumberHElements;
int hrows=`modelNumberOfEquations`;
int hcols=`modelColumns`*`modelNumberOfEquations`;
double okay[`spaceForTempVars`];
if(*homotopyAlpha>=1.0) {
`opVarDefsDrvSFA`


`sparseFunctionDerivativeAssignmentsA`
`sparseFunctionDerivativeAssignmentsIA`
`sparseFunctionDerivativeAssignmentsJA`
} else {
`opLinVarDefsDrvSFA`
`linSparseFunctionDerivativeAssignmentsA`
`linSparseFunctionDerivativeAssignmentsIA`
`linSparseFunctionDerivativeAssignmentsJA`
/*initialize cMat to zero sparse matrix*/
for(i=0;i<=`modelNumberOfEquations`+1;i++){icMat[i]=1;}
for(i=0;i<`bLength`;i++){cMat[i]=0;};
if(*homotopyAlpha>0.0) {
`opNLinVarDefsDrvSFA`
`nlinSparseFunctionDerivativeAssignmentsA`
`nlinSparseFunctionDerivativeAssignmentsIA`
`nlinSparseFunctionDerivativeAssignmentsJA`
for(i=0;i<`bLength`;i++){cMat[i]=cMat[i]*(*homotopyAlpha);};
}
maxNumberHElements=`bLength`;
aplb_(&hrows,&hcols,&aOne,bMat,jbMat,ibMat,cMat,jcMat,icMat,
aMat,jaMat,iaMat,&maxNumberHElements,okay,&ierr);
}
}
"



writeMakefile[outFile_String,aList_Association]:=
WriteString[outFile<>"Makefile",
	TemplateApply[makefileTemplate,aList]]



makefileTemplate="
LAPACK  = -L/opt/atlas/lib/ -lcblas -lf77blas -latlas -llapack
CSTOCHSIMSDIR = ../CStochSims/
SPAMADIR = ../sparseAMA

RANLIBLOC = $(CSTOCHSIMSDIR)/ranlib.o
DEBRANLIBLOC = $(RANLIBLOC)


SPAMALIB = $(CSTOCHSIMSDIR)
DEBSPAMALIB = $(CSTOCHSIMSDIR)

STACKLIB = $(CSTOCHSIMSDIR)
DEBSTACKLIB = $(CSTOCHSIMSDIR)

STOCHLIB = $(CSTOCHSIMSDIR)
DEBSTOCHLIB = $(CSTOCHSIMSDIR)

CFLAGS = -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc

LINKFLAGS =   -v  -lc -ldl -lm -L../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared -lsparseAMA-1.0-SNAPSHOT $(CSTOCHSIMSDIR)stackC.o $(CSTOCHSIMSDIR)stochProto.o

DEBLINKFLAGS =   -v  -lc -ldl -lm -L../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared -lsparseAMA-1.0-SNAPSHOT $(CSTOCHSIMSDIR)debStackC.o $(CSTOCHSIMSDIR)debStochProto.o

.SUFFIXES:	.o .c .h




deb`outFile`.o:	`outFile`.c
	gcc $(CFLAGS) -g -pg -o deb`outFile`.o `outFile`.c
deb`outFile`Drv.o:	`outFile`Drv.c
	gcc $(CFLAGS) -g -pg -o deb`outFile`Drv.o `outFile`Drv.c
deb`outFile`Support.o:	`outFile`Support.c
	gcc $(CFLAGS) -g -pg -o deb`outFile`Support.o `outFile`Support.c
deb`outFile`Data.o:	`outFile`Data.c
	gcc $(CFLAGS) -g -pg -o deb`outFile`Data.o `outFile`Data.c
deb`outFile`Shocks.o:	`outFile`Shocks.c
	gcc $(CFLAGS) -g -pg -o deb`outFile`Shocks.o `outFile`Shocks.c
debrun`outFile`.o:	run`outFile`.c  run`outFile`LocalDefs.h
	gcc $(CFLAGS) -g -pg -o debrun`outFile`.o run`outFile`.c 




`outFile`.o:	`outFile`.c
	gcc $(CFLAGS) -O4  -o `outFile`.o `outFile`.c
`outFile`Drv.o:	`outFile`Drv.c
	gcc $(CFLAGS) -O4  -o `outFile`Drv.o `outFile`Drv.c
`outFile`Support.o:	`outFile`Support.c
	gcc $(CFLAGS) -O4  -o `outFile`Support.o `outFile`Support.c
`outFile`Data.o:	`outFile`Data.c
	gcc $(CFLAGS) -O4  -o `outFile`Data.o `outFile`Data.c
`outFile`Shocks.o:	`outFile`Shocks.c
	gcc $(CFLAGS) -O4  -o `outFile`Shocks.o `outFile`Shocks.c


run`outFile`.o:	run`outFile`.c run`outFile`LocalDefs.h
	gcc $(CFLAGS) -O4 run`outFile`.c 

mpirun`outFile`.o:	mpirun`outFile`.c run`outFile`LocalDefs.h
	gcc $(CFLAGS) -O4 mpirun`outFile`.c 


run`outFile`:	`outFile`.o run`outFile`.o \
	`outFile`Drv.o `outFile`Support.o \
	`outFile`Data.o `outFile`Shocks.o 
	gfortran -o run`outFile` -O4 `outFile`.o run`outFile`.o \
	`outFile`Drv.o `outFile`Support.o \
	`outFile`Data.o `outFile`Shocks.o \
	  $(RANLIBLOC) $(CSTOCHSIMSDIR)myNewt.o ../CStochSims/ma50ad.o\
		-v  -lc -ldl -lm  $(LINKFLAGS) $(LAPACK) 
		@echo \"to run requires runtime mod-> LD_LIBRARY_PATH=../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared:$(LD_LIBRARY_PATH)\"




debrun`outFile`:	deb`outFile`.o debrun`outFile`.o \
	deb`outFile`Drv.o deb`outFile`Support.o \
	deb`outFile`Data.o deb`outFile`Shocks.o 
	gfortran -g -o debrun`outFile`  deb`outFile`.o debrun`outFile`.o \
	deb`outFile`Drv.o deb`outFile`Support.o \
	deb`outFile`Data.o deb`outFile`Shocks.o \
	  $(RANLIBLOC) $(CSTOCHSIMSDIR)debMyNewt.o ../CStochSims/ma50ad.o\
		-v  -lc -ldl -lm  $(DEBLINKFLAGS) $(LAPACK) 
		@echo \"to run requires runtime mod-> LD_LIBRARY_PATH=../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared:$(LD_LIBRARY_PATH)\"

"

writeMPIRun[outFile_String,aList_Association]:=
WriteString["runmpi"<>outFile<>".c",
	TemplateApply[mpiRunTemplate,aList]]

mpiRunTemplate="

/*Mathematica Creation Date`date`*/
/*`modelCreationInfo`*/
#include \"runItExternalDefs.h\"
#include \"distStochSims.h\"
main(int argc, char * argv[])
{
#include \"runItInvariantLocalDefs.h\"
#include \"run`outFile`LocalDefs.h\"
#include \"runItInvariantMpiDefs.h\"
printf(\"$Id: mpiRunIt.mc,v 1.1 2001/06/19 19:49:23 m1gsa00 Exp m1gsa00 $\n\");



  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numberOfProcesses);
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  MPI_Get_processor_name(processorName, &nameLength);

/*obtain dimensions for model*/
`functionName`ModelDimensions(&numberOfEquations,&lags,&leads,
	&numberOfParameters,&numberOfDataValues,&numberOfShocks,&numberExog);

/*allocate space for objects that do not depend on command line switches*/

allocLinearTerminator(numberOfEquations,lags,leads,
numberExog,
maxNumberElements,
&upsilonMatrix,&upsilonMatrixj,&upsilonMatrixi,
&hMat,&hMatj,&hMati,
&hzMat,&hzMatj,&hzMati,
&cstar,&cstarj,&cstari,
&AMqMatrix,&AMqMatrixj,&AMqMatrixi,
& rootr,&rooti,
&AMbMatrix,&AMbMatrixj,&AMbMatrixi,
&phiInvMat,&phiInvMatj,&phiInvMati,
&fmat,&fmatj,&fmati,
&varthetaC,&varthetaCj,&varthetaCi,
&varthetaZstar,&varthetaZstarj,&varthetaZstari
);
/*space for data and shocks*/
allocShocksData(numberOfEquations,numberOfShocks,numberOfDataValues,
	&`functionName`ShockVals,&`functionName`DataVals,
	&`functionName`ZeroShock);
/*space for qmatrix*/
/*allocAltComputeAsymptoticQ(numberOfEquations,lags,leads,spaMaxNumberElements,
	&AMqMatrix,&AMqMatrixj,&AMqMatrixi,&rootr,&rooti);*/
/*space for bmatrix*/
/*allocAltComputeAsymptoticQ(numberOfEquations,lags,leads,spaMaxNumberElements,
	&AMbMatrix,&AMbMatrixj,&AMbMatrixi,&brootr,&brooti);*/
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf(\"after storage allocations\n totalTime=%f,
	userSystemTime=%f,systemTime=%f\n\",
	*totalTime,*userSystemTime,*(userSystemTime+1));



/*initialize data and shocks*/
for(i=0;i<numberOfDataValues;i++){`functionName`Data(i,
	`functionName`DataVals+(i*numberOfEquations));}
for(i=0;i<numberOfShocks;i++){`functionName`Shocks(i,
	`functionName`ShockVals+(i*numberOfEquations));}


processCommandLine(argc,argv,namesArray,numberOfEquations,
paramNamesArray,numberOfParameters,parameters,
	`functionName`DataVals,numberOfDataValues,
	&pathLength,&replications,&t0,&stochasticPathLength,
intControlParameters,doubleControlParameters,flnm);


/*open output file*/
    outFile=fopen(flnm,\"w\");



/*allocate space for objects that depend on command line switches*/
allocMa50(numberOfEquations,lags,leads,pathLength,pathLength*MAXELEMENTS,
		  &ma50bdIptru,
		  &ma50bdIptrl,
		  &ma50bdIrnf,
		  &ma50bdFact,
		  &ma50bdIq,
		  &ma50bdJob);
allocMa50(numberOfEquations,lags,leads,1,MAXELEMENTS,
		  &cmpma50bdIptru,
		  &cmpma50bdIptrl,
		  &cmpma50bdIrnf,
		  &cmpma50bdFact,
		  &cmpma50bdIq,
		  &cmpma50bdJob);
/*space for FP and for newton step workspace*/
allocFPNewt(numberOfEquations,lags,leads,
	pathLength,MAXELEMENTS,
&`functionName`FP,
&`functionName`Intercept,
&fmats,&fmatsj,&fmatsi,&smats,&smatsj,&smatsi);
/*space for path*/
allocPathNewt(numberOfEquations,lags,leads,
	pathLength,replications,stochasticPathLength,
&`functionName`PathQ,&`functionName`ZeroPathQ);
/*space for stochSims sucess record*/
allocStochSims(stochasticPathLength,replications,&failedQ);
/*initialize  whole path to data values at t0*/
for(i=0;i<lags+pathLength+leads+stochasticPathLength;i++){
  for(j=0;j<numberOfEquations;j++){
	`functionName`ZeroPathQ[i* numberOfEquations+j]=
	  `functionName`DataVals[(i+t0)*numberOfEquations+j];
	`functionName`PathQ[i* numberOfEquations+j]=
	  `functionName`DataVals[(i+t0)*numberOfEquations+j];
  }}



/*initialize  whole path to data values at t0*/
for(i=0;i<lags+1+leads;i++){
  for(j=0;j<numberOfEquations;j++){
	`functionName`FP[i* numberOfEquations+j]=
	  `functionName`DataVals[(i+pathLength+t0)*numberOfEquations+j];
  }}








/*
`functionName`PeriodicPointGuesser(parameters,1,`functionName`FP);

printf(\"initiating FP solution computation\n\");
FPnewt(&numberOfEquations,&lags,&leads,
`functionName`,`functionName`Derivative,parameters,
`functionName`FP,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo);
printf(\"computed FP solution\n\");
*/
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf(\"after fixed point computation\n totalTime=%f,userSystemTime=%f,systemTime=%f\n\",*totalTime,*userSystemTime,*(userSystemTime+1));






printf(\"generating perm vec\n\");
allocGenerateDraws(1,stochasticPathLength,replications,
&`functionName`PermVec);
 generateDraws(1,(stochasticPathLength),replications,numberOfShocks,`functionName`PermVec);
printf(\"done generating perm vec\n\");

/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf(\"after generating draws\n totalTime=%f,userSystemTime=%f,
systemTime=%f\n\",*totalTime,*userSystemTime,*(userSystemTime+1));








/*compute asymptotic Q constraint*/
if(!useIdentityQ){
altComputeAsymptoticQMatrix(
&numberOfEquations,&lags,&leads,
`functionName`,`functionName`Derivative,parameters,
`functionName`FP,pathLength,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&spaMaxNumberElements,
AMqMatrix,AMqMatrixj,AMqMatrixi,&auxInit,&qRows,rootr,rooti,
failedQ,0,
 intControlParameters, doubleControlParameters,
 intOutputInfo,  doubleOutputInfo
);
/*
`functionName`Upsilon(parameters,
upsilonMatrix,upsilonMatrixj,upsilonMatrixi
			  );
linearTerminator(&numberOfEquations,&lags,&leads,
&numberExog,`functionName`FP,
`functionName`,`functionName`Derivative,`functionName`ExogH,
parameters,
upsilonMatrix,upsilonMatrixj,upsilonMatrixi,
&spaMaxNumberElements,
hMat,hMatj,hMati,
hzMat,hzMatj,hzMati,
cstar,cstarj,cstari,
AMqMatrix,AMqMatrixj,AMqMatrixi,
&auxInit,&qRows,rootr,rooti,
AMbMatrix,AMbMatrixj,AMbMatrixi,
phiInvMat,phiInvMatj,phiInvMati,
fmat,fmatj,fmati,
varthetaC,varthetaCj,varthetaCi,
varthetaZstar,varthetaZstarj,varthetaZstari,
failedQ
);


*/

fprintf(outFile,\"`functionName`AuxInitQRows={%d,%d}\n\",auxInit,qRows);
fPrintMathDbl(outFile,(numberOfEquations*(leads+lags)),
      rootr,\"`functionName`Rootr\");
fPrintMathDbl(outFile,(numberOfEquations*(leads+lags)),
      rooti,\"`functionName`Rooti\");

/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf(\"after computing Q,max elems=%d\n totalTime=%f,userSystemTime=%f,
systemTime=%f\n\",spaMaxNumberElements,*totalTime,*userSystemTime,*(userSystemTime+1));
}

if(qRows <numberOfEquations*leads){
printf(\"qmatrix has %d rows, need %d so using identity matrix terminal condition\n\",qRows,numberOfEquations*leads);
altComputeAsymptoticIMatrix(
&numberOfEquations,&lags,&leads,
AMqMatrix,AMqMatrixj,AMqMatrixi,
failedQ
);
}

/*
spaMaxNumberElements=SPAMAXELEMENTS;
obtainSparseReducedForm(&spaMaxNumberElements,numberOfEquations* leads,
numberOfEquations* (leads+lags),
AMqMatrix,AMqMatrixj,AMqMatrixi,
AMbMatrix,AMbMatrixj,AMbMatrixi);*/
/*time used so far?*/
/* *totalTime=dtime_(userSystemTime);
printf(\"after computing B\n totalTime=%f,userSystemTime=%f,
systemTime=%f\n\",*totalTime,*userSystemTime,*(userSystemTime+1));
*/








/*use reduced form to compute rest of path*/
/*for(i=0;i< pathLength+leads+stochasticPathLength;i++){
	applySparseReducedForm(numberOfEquations,
		numberOfEquations* lags,
		`functionName`ZeroPathQ+(i *numberOfEquations),`functionName`FP,
		AMbMatrix,AMbMatrixj,AMbMatrixi,
`functionName`ZeroPathQ+((i * numberOfEquations)+(numberOfEquations*lags)));
for(j=0;j<numberOfEquations;j++){
`functionName`ZeroPathQ[((i * numberOfEquations)+(numberOfEquations*lags))+j]=
`functionName`FP[j+(numberOfEquations*lags)%
	(numberOfEquations*(lags+leads+1))];}
}*/

/*set terminal time and call stochSim*/
tf=t0+stochasticPathLength-1;
printf(\"saving values for variable in file named %s\n\",flnm);
fprintf(outFile,\"`functionName`RunParams={%d,%d,%d,%d,%d,%d,%d};\n\",
    numberOfEquations,lags,leads,
     pathLength,t0,stochasticPathLength,replications);
fPrintMathInt(outFile,replications * (stochasticPathLength),
      `functionName`PermVec,\"`functionName`PermVec\");
fPrintMathDbl(outFile,(numberOfEquations*numberOfDataValues),`functionName`DataVals,\"`functionName`Data\");
fPrintMathDbl(outFile,(numberOfEquations*numberOfShocks),`functionName`ShockVals,\"`functionName`Shocks\");



/*begin biiiiiiiiiig MPI paste*/
  if (myRank == 0)
    {
      startwtime = MPI_Wtime();
      printf(\"Process #%d: %d replications.\n\", myRank, replications);
    }

  printf(\"Process %d has been started on host %s\n\", myRank, processorName);

  pathQLength = replications * numberOfEquations *
    (lags + leads + pathLength + stochasticPathLength);
  failedQLength = replications;
  buildResultType(`functionName`PathQ, failedQ, &completedDraw, pathQLength,
		  failedQLength, &resultMessageType);

  if ((myRank == 0) && (replications < numberOfProcesses - 1))
    {
      printf(\"Process #%d: Too many processes for the number of replications. Killing some 
processes.\n\",
	     myRank);
      for (i = replications + 1; i < numberOfProcesses; i++)
	{
	  sendHaltMessage(i);
	}
      numberOfProcesses = replications + 1;
    }

  if (myRank == 0)
    {
      printf(\"Process #0: pid = %d\n\", getpid());
      for (newDraw = 0; newDraw < numberOfProcesses - 1; newDraw++)
	{
	  destination = newDraw + 1;
	  sendDataMessage(destination, &newDraw);
	}

      for (numberOfCompletedDraws = 0; numberOfCompletedDraws < replications;
	   numberOfCompletedDraws++)
	{
	  printf(\"Process #0: About to start waiting for message\n\"); fflush(stdout);
	  MPI_Recv(`functionName`PathQ, 1, resultMessageType, MPI_ANY_SOURCE,
		   RESULT_MSG_TAG, MPI_COMM_WORLD, &status);
	  printf(\"Process #0: Received message\n\"); fflush(stdout);
	  source = status.MPI_SOURCE;
	  if (status.MPI_ERROR != 0)
	    error(myRank, source, status.MPI_TAG, status.MPI_ERROR);
	  printf(\"Process #0 received results of replication %d from process #%d.\n\",
		 completedDraw, source); fflush(stdout);

	  /**************
	  writeOutput(flnm, completedDraw, `functionName`PathQ, failedQ, pathQLength,
		      failedQLength, pathLength, t0, stochasticPathLength,
		      replications, `functionName`PermVec, `functionName`DataVals,
		      `functionName`ShockVals, lags, leads, numberOfEquations, numberOfShocks,
		      numberOfDataValues);
		      **************/
	  if (numberOfCompletedDraws < replications - numberOfProcesses + 1)
	    {
	      destination = source;
	      sendDataMessage(destination, &newDraw);
	      newDraw++;
	    }
	  else /*** tell other process that we're done ***/
	    {
	      destination = source;
	      sendHaltMessage(destination);
	    }
	}
      printf(\"Process #%d: All replications have completed.\n\", myRank); fflush(stdout);
      endwtime = MPI_Wtime();
      printf(\"Process #%d: Wall clock time = %f.\n\", myRank, endwtime - startwtime);
      fflush(stdout);
    }
  else /*** myRank not 0 ***/
    {
      int halt = 0;

      printf(\"Process #%d: pid = %d\n\", myRank, getpid());

      while (!halt)
	{
	  source = 0;
	  printf(\"Process #%d: About to start waiting for message\n\", myRank); fflush(stdout);
	  MPI_Recv(buffer, BUFFER_SIZE, MPI_PACKED, source, MPI_ANY_TAG, MPI_COMM_WORLD,
		   &status);
	  printf(\"Process #%d: Received message\n\", myRank); fflush(stdout);
	  printf(\"Process #%d: About to check for errors. source = %d, tag = %d, status. MPIERROR = 
%d.\n\",
		 myRank, source, tag, status.MPI_ERROR); fflush(stdout);
	  tag = status.MPI_TAG;
	  if (status.MPI_ERROR != 0)
	    error(myRank, source, tag, status.MPI_ERROR);
	  fflush(stderr);
	  printf(\"Process #%d: Completed error check.\n\", myRank); fflush(stdout);

	  if (tag == HALT_MSG_TAG)
	    halt = 1;
	  else
	    {
	      position = 0;
	      MPI_Unpack(buffer, BUFFER_SIZE, &position, &newDraw, 1, MPI_INT,
			 MPI_COMM_WORLD);

	      sprintf(outFileName, \"%s%d\", flnm, newDraw);
	      outFile=fopen(outFileName,\"w\");

	      /*set terminal time and call stochSim*/
	      tf=t0+stochasticPathLength-1;

	      printf(\"Process #%d: saving values for variable in file named 
%s\n\",myRank,outFileName);
	      fprintf(outFile,\"`functionName`RunParams={%d,%d,%d,%d,%d,%d,%d};\n\",
		      numberOfEquations,lags,leads,
		      pathLength,t0,stochasticPathLength,replications);
	      fPrintMathInt(outFile,replications * (stochasticPathLength),
			    `functionName`PermVec,\"`functionName`PermVec\");
	      /************************  these generate too much output for frbus
	      
fPrintMathDbl(outFile,(numberOfEquations*numberOfDataValues),`functionName`DataVals,\"`functionName`Data\");
	      
fPrintMathDbl(outFile,(numberOfEquations*numberOfShocks),`functionName`ShockVals,\"`functionName`Shocks\");
/*end biiiiiiiiiig MPI paste*/

*ma50bdJob=1;
pathNewt(&numberOfEquations,&lags,&leads,&pathLength,
`functionName`,`functionName`Derivative,parameters,
`functionName`ZeroShock,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,AMqMatrix,AMqMatrixj,AMqMatrixi,
`functionName`FP,`functionName`Intercept,`functionName`FP,
`functionName`ZeroPathQ,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo,
ma50bdJob,
ma50bdIq,
ma50bdFact,
ma50bdIrnf,
ma50bdIptrl,
ma50bdIptru);
fPrintMathInt(outFile,widthIntOutputInfo,intOutputInfo,
\"`functionName`ZeroShockIntOutputInfo\");
fPrintMathDbl(outFile,widthIntOutputInfo,doubleOutputInfo,
\"`functionName`ZeroShockDoubleOutputInfo\");


fPrintMathDbl(outFile,(numberOfEquations*(leads+pathLength+lags)),
      `functionName`ZeroPathQ,\"`functionName`ZeroShockResults\");
fPrintMathInt(outFile,1,
failedQ,\"`functionName`ZeroShocksFailedQ\");
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf(\"after using Q matrix\ntotalTime=%f,userSystemTime=%f,
systemTime=%f\n\",*totalTime,*userSystemTime,*(userSystemTime+1));
*cmpma50bdJob=1;
diststochSim(&numberOfEquations,&lags,&leads,&pathLength,
`functionName`,`functionName`Derivative,parameters,
&replications,&t0,&tf,`functionName`PermVec,
`functionName`ShockVals,&numberOfShocks,
`functionName`DataVals,&numberOfDataValues,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,AMqMatrix,AMqMatrixj,AMqMatrixi,
`functionName`ZeroPathQ,`functionName`Intercept,`functionName`FP,
`functionName`PathQ,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo,
ma50bdJob,
ma50bdIq,
ma50bdFact,
ma50bdIrnf,
ma50bdIptrl,
ma50bdIptru,
cmpma50bdJob,
cmpma50bdIq,
cmpma50bdFact,
cmpma50bdIrnf,
cmpma50bdIptrl,
cmpma50bdIptru,newDraw,outFile
);
fPrintMathDbl(outFile,(replications * numberOfEquations*(stochasticPathLength+lags)),
      `functionName`PathQ,\"`functionName`Results\");
fPrintMathInt(outFile,replications*widthIntOutputInfo,intOutputInfo,
\"`functionName`IntOutputInfo\");
fPrintMathDbl(outFile,replications*widthIntOutputInfo,doubleOutputInfo,
\"`functionName`DoubleOutputInfo\");
fPrintMathInt(outFile,replications*stochasticPathLength,
failedQ,\"`functionName`failedQ\");
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf(\"after using Q matrix\ntotalTime=%f,userSystemTime=%f,
systemTime=%f\n\",*totalTime,*userSystemTime,*(userSystemTime+1));
	      writeOutput(outFile, newDraw, `functionName`PathQ, failedQ, pathQLength,
			  failedQLength, pathLength, t0, stochasticPathLength,
			  replications, `functionName`PermVec, `functionName`DataVals,
			  `functionName`ShockVals, lags, leads, numberOfEquations, numberOfShocks,
			  numberOfDataValues);

	      printf(\"Process #%d: Finished stochSim().\n\", myRank); fflush(stdout);

	      destination = 0;
	      tag = RESULT_MSG_TAG;
	      completedDraw = newDraw;
	      printf(\"Process #%d sending replication %d results to process #0.\n\",
		     myRank, completedDraw);
	      MPI_Send(`functionName`PathQ, 1, resultMessageType, destination,
		       tag, MPI_COMM_WORLD);
	      fclose(outFile);
	    }
}}

  MPI_Finalize();






cfreeLinearTerminator(
&upsilonMatrix,&upsilonMatrixj,&upsilonMatrixi,
&hMat,&hMatj,&hMati,
&hzMat,&hzMatj,&hzMati,
&cstar,&cstarj,&cstari,
&AMqMatrix,&AMqMatrixj,&AMqMatrixi,
& rootr,&rooti,
&AMbMatrix,&AMbMatrixj,&AMbMatrixi,
&phiInvMat,&phiInvMatj,&phiInvMati,
&fmat,&fmatj,&fmati,
&varthetaC,&varthetaCj,&varthetaCi,
&varthetaZstar,&varthetaZstarj,&varthetaZstari
);

cfreeGenerateDraws(&`functionName`PermVec);
cfreeShocksData(&`functionName`ShockVals,&`functionName`DataVals,
	&`functionName`ZeroShock);
/*cfreeAltComputeAsymptoticQ(
&AMqMatrix,&AMqMatrixj,&AMqMatrixi,&rootr,&rooti);
cfreeAltComputeAsymptoticQ(
&AMbMatrix,&AMbMatrixj,&AMbMatrixi,&brootr,&brooti);*/
cfreePathNewt(&`functionName`PathQ);
cfreePathNewt(&`functionName`ZeroPathQ);
cfreeFPNewt(lags,pathLength,
&`functionName`FP,
&`functionName`Intercept,
&fmats,&fmatsj,&fmatsi,&smats,&smatsj,&smatsi);
cfreeStochSims(&failedQ);
cfreeMa50(&ma50bdIptru,
		  &ma50bdIptrl,
		  &ma50bdIrnf,
		  &ma50bdFact,
		  &ma50bdIq,
		  &ma50bdJob);
cfreeMa50(&cmpma50bdIptru,
		  &cmpma50bdIptrl,
		  &cmpma50bdIrnf,
		  &cmpma50bdFact,
		  &cmpma50bdIq,
		  &cmpma50bdJob);


return(0);

}

#define SHOCKS `shocksCols`
void modData(int numberOfEquations,int numberDataValues,double * dataVals,
			 int vbl,int t0,int tf,double val1,double val2)
{
  int t;
  for(t=t0;t<=tf&&t<numberDataValues;t++){
dataVals[t*numberOfEquations+vbl]=dataVals[t*numberOfEquations+vbl]+
  (t-t0)*val2/(tf-t0) + (tf-t)*val1/(tf-t0);
  }
}
void modDataAbs(int numberOfEquations,int numberDataValues,double * dataVals,
			 int vbl,int t0,int tf,double val1,double val2)
{
  int t;
  for(t=t0;t<=tf&&t<numberDataValues;t++){
dataVals[t*numberOfEquations+vbl]=(t-t0)*val2/(tf-t0) + (tf-t)*val1/(tf-t0);
  }
}
 
 
#include \"runItOther.h\"

"
writeDataInclude[outFile_String,aList_Association]:=
WriteString[outFile<>"DataForInclude.h",
	TemplateApply[dataIncludeTemplate,aList]]

dataIncludeTemplate=
"

/*Mathematica Creation Date`date`*/
static double theData[`dataRows`][`dataCols`]=
`vstr`;




"
writeShocksInclude[outFile_String,aList_Association]:=
WriteString[outFile<>"ShocksForInclude.h",
	TemplateApply[shocksIncludeTemplate,aList]]

shocksIncludeTemplate=
"

/*Mathematica Creation Date`date`*/
static double theShocks[`shocksRows`][`shocksCols`]=
`dstr`;



"
writeShocks[outFile_String,aList_Association]:=
WriteString[outFile<>"Shocks.c",
	TemplateApply[shocksTemplate,aList]]

shocksTemplate=
"
/*`dvalsInfo`*/
void `functionName`Shocks(int t,double * vectorOfVals)
{
int i;
#include \"`outFile`ShocksForInclude.h\"/*dstr;*/
for(i=0;i<`shocksCols`;i++)vectorOfVals[i]=0;
for(i=0;i<`modelNumberOfEquations`-`numbExog`;i++)vectorOfVals[i]=theShocks[t][i];
}


"

writeRunLocalDefs[outFile_String,aList_Association]:=
WriteString["run"<>outFile<>"LocalDefs.h",
	TemplateApply[runItLocalDefsTemplate,aList]]

runItLocalDefsTemplate=
"



/*Mathematica Creation Date`date`*/
#define SPAMAXELEMENTS 15*(`modelNumberOfEquations`)^2
#define MAXELEMENTS 25*(`modelNumberOfEquations`)^2

int  maxNumberElements=MAXELEMENTS;
int  spaMaxNumberElements=SPAMAXELEMENTS;
void `functionName`(double * xvec,double * pvec,double * shock,
double * alhs,
int * jalhs,int * ialhs,int * alphas,double * linPt
);
void `functionName`Derivative(double * xvec,double * pvec,
double * alhs,
int * jalhs,
int * ialhs);
void `functionName`ExogH(double * pvec,
double * alhs,
int * jalhs,
int * ialhs);
/*model specific names and data*/
char * namesArray[] =  
`theNamesArray`;
char * paramNamesArray[] =  
`theParamNamesArray`;
double parameters[]=
`defaultParams`;
int `functionName`exogQ[]=
`exogQ`;

int * `functionName`PermVec;
double * `functionName`ZeroShock;
double * `functionName`ShockVals;
double * `functionName`DataVals;
double * `functionName`FP;
double * `functionName`Intercept;
double * `functionName`EasyPathQ;
double * `functionName`TargetPathQ;
double * `functionName`PathQ;
double * `functionName`ZeroPathQ;
"

writeData[outFile_String,aList_Association]:=
WriteString[outFile<>"Data.c",
	TemplateApply[dataTemplate,aList]]

dataTemplate=
"



/*`valsInfo`*/
void `functionName`Data(int t,double * vectorOfVals)
{
int i;
#include  \"`outFile`DataForInclude.h\"/*vstr;*/
for(i=0;i<`dataCols`;i++)vectorOfVals[i]=theData[t][i];
}

"

writeRun[outFile_String,aList_Association]:=
WriteString["run"<>outFile<>".c",
	TemplateApply[runItTemplate,aList]]

runItTemplate=
"



/*Mathematica Creation Date`date`*/
/*`modelCreationInfo`*/
#include <stdlib.h>
#include \"`runItExt`\"



#define PATHLENGTH 1000

int numberOfEquations=`modelNumberOfEquations`;
char * namesArray[] =  `theNamesArray`;
char * paramNamesArray[] = `theParamNamesArray`;
int numberOfParameters=`numberOfParameters`;
int * parameters[]={};
int numDATA=`dataRows`;
int numSHOCKS=`shocksRows`;
double * theData;

main(int argc, char * argv[])
{
#include \"`runItInv`\"
#include \"run`outFile`LocalDefs.h\"
printf(\" runIt.mc, 2016 m1gsa00 \\n\");

`functionName`DataVals=(double *)calloc(numberOfEquations*numDATA,sizeof(double));
for(i=0;i<numDATA;i++){rbcExampleData(i,`functionName`DataVals+(i*numberOfEquations));}

`functionName`ShockVals=(double *)calloc(numberOfEquations*numSHOCKS,sizeof(double));
for(i=0;i<numSHOCKS;i++){rbcExampleShocks(i,`functionName`ShockVals+(i*numberOfEquations));}


processCommandLine(argc,argv,namesArray,numberOfEquations,
paramNamesArray,numberOfParameters,parameters,
	`functionName`DataVals,numDATA,numSHOCKS,
	&pathLength,&replications,&t0,&stochasticPathLength,
intControlParameters,doubleControlParameters,flnm);

/*
`functionName`PeriodicPointGuesser(parameters,1,`functionName`FP);

FPnewt(numberOfEquations,lags,leads,
`functionName`,`functionName`Derivative,parameters,
`functionName`FP,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
maxNumberElements,
failedQ);

*/
}

#include \"`runItOth`\"

/*
printf(\"generating perm vec\n\");
 generateDraws(1,(stochasticPathLength),(*replications),numSHOCKS,julliardPermVec);
printf(\"done generating perm vec\n\");
*/


/*


altComputeAsymptoticQMatrix(
numberOfEquations,lags,leads,
`functionName`,`functionName`Derivative,parameters,
`functionName`FP,pathLength,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
maxNumberElements,
AMqMatrix,AMqMatrixj,AMqMatrixi,
failedQ
);
*/

/*
if(failedQ[0])
{  printf(\"problems computing  Q matrix\n\");return(1);}
else {printf(\"computed Q matrix\n\");}
*//*
printf(\"computed Q matrix\n\");
for(i=0;i< *pathLength;i++){
`functionName`PeriodicPointGuesser(parameters,1,
julliardPathQ+(i *julNEQS));}
*totalTime=dtime(userSystemTime);
printf(\"after computing Q matrix\ntotalTime=%f,userSystemTime=%f,systemTime=%f\n\",
*totalTime,*userSystemTime,*(userSystemTime+1));
printf(\"using q matrix\n\");*/
/*

stochSim(numberOfEquations,lags,leads,pathLength,
`functionName`,`functionName`Derivative,parameters,
replications,t0,tf,julliardPermVec,
julliardShocks,numberOfShocks,
julliardData,numberOfData,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
maxNumberElements,AMqMatrix,AMqMatrixj,AMqMatrixi,
julliardFP,
julliardPathQ,
failedQ);

*/

"

writeCSupport[outFile_String,aList_Association]:=
WriteString[outFile<>"Support.c",
	TemplateApply[cSupportTemplate,aList]]

cSupportTemplate="


/*Mathematica Creation Date`date`*/
/*`modelCreationInfo`*/
#include \"`lagLeadLoc`\"
#include <math.h>
static double maxarg1,maxarg2;
#include <math.h>

double FMAX(double a,double b)
{
  return(a > b ? a : b);
}
double FMIN(double a,double b)
{
  return(a < b ? a : b);
}
double FABS(double a)
{
  return(a > 0 ? a: -a);
}

double doRightSmaller(double a,double b)
{
  return(a < b ? 0 : 1);
}
double doSign(double a)
{
  /*  return(fabs(a) >0.01?(a > 0 ? 1 : -1):2*a) ;*/
  return(a > 0 ? 1 : -1) ;

}
#define modelShock(n) (0)  


`stateVectorDefines`
`coeffDefines`  




void `functionName`PeriodicPointGuesser
(double * parameters,int period,
	double guessVector[`modelColumns`][`modelNumberOfEquations`])
{
int i,j;
double svalue;
int timeOffset;
for(timeOffset=0;
	timeOffset<period+ `modelColumns` - 1;
			timeOffset++)
	{
`periodPointGuesserAssignments`
}
}

void `functionName`ModelDimensions(int * numberOfEquations, int * lags,
int * leads, int * numberOfParameters,
int * numberOfDataValues, int * numberOfShocks,int * numberExogenous)
{
*numberOfEquations=`modelNumberOfEquations`;
*lags=`lags`;
*leads=`leads`;
*numberOfParameters=`numberOfParameters`;
*numberOfDataValues=`dataRows`;
*numberOfShocks=`shocksRows`;
*numberExogenous=`numbExog`;
}
void `functionName`Upsilon(double *parameters,
double * aMat,int * jaMat,int *iaMat
)
{
`upsilonMatA`
`upsilonMatIA`
`upsilonMatJA`
}
void `functionName`ExogH(double *parameters,double *stateVector,
double * aMat,int * jaMat,int *iaMat
)
{
`exogHMatA`
`exogHMatIA`
`exogHMatJA`
}
void `functionName`SelectZ(double * aMat,int * jaMat,int *iaMat
)
{
`selectZMatA`
`selectZMatIA`
`selectZMatJA`
}
"
End[] (* End Private Context *)

EndPackage[]
