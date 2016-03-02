(* Wolfram Language Package *)

BeginPackage["Stack`", { "AsymptoticLinearization`", "SymbolicAMA`","ProtectedSymbols`"}]
(* Exported symbols added here with SymbolName::usage *)  

modelPrep::usage=
"modelPrep[mod_]->{mod[x],dmod[x]} where x corresponds to\n
the model (nlag+1+nlead)*neq endogenous variables" 


lagLead::usage=
"lagLead[eqns_]"

eqnVars::usage=
"eqnVars[eqns_]"


nxtCDmats::usage=
"nxtCDmats[{lags_,smats_,fvec_,cmats_,dmats_}]"



backSub::usage=
"backSub[cmats_,dmats_]"

oneStepBack::usage=
"oneStepBack[{yvec_,rcmats_,rdvecs_}]"


denseToSparseRow::usage=
"denseToSparseRow[mat_List]"
denseToSparseMat::usage=
"denseToSparseMat[mat_List]"

nxtGuess::usage=
"nxtGuess[lags_,theFunc_,theDrvFunc_,termConstr_,fp_]"

Global`t::usage=
"time t, if this appears in a function arg, 
then stack treats that functions name as  a variable name"


Begin["private`"]




riffle[cmatList_,dmatList_,smat_,fvec_]:=
If[cmatList === {},{smat,fvec},
With[{cmatNow=cmatList[[1]],cmatRest=Rest[cmatList],
dmatNow=dmatList[[1]],dmatRest=Rest[dmatList]},
With[{seqns=Length[smat],
eqns=Length[cmatNow],scols=Length[smat[[1]]],ccols=Length[cmatNow[[1]]]},
With[{expand=BlockMatrix[{{cmatList[[1]],
      ZeroMatrix[eqns,scols-ccols-eqns]}}],
smatSub=SubMatrix[smat,{1,1},{seqns,eqns}] },
Apply[riffle,{cmatRest,dmatRest,SubMatrix[smat,{1,eqns+1},{seqns,scols-eqns}] -
  smatSub. expand,fvec - smatSub . dmatNow}]]]]]



eqnVars[eqns_]:=Union[Cases[
eqns,x_[Global`t]->x,Infinity],Cases[eqns,x_[Global`t+_]->x,Infinity]]



lagLead[eqns_]:=
With[{lagLead=Union[Cases[eqns,_[Global`t]->0,{Infinity}],Cases[eqns,_[Global`t+x_]->x,Infinity]]},
With[{maxLag=-Min[lagLead],maxLead=Max[lagLead]},
{maxLag,maxLead}]]



nxtCDmats[{lags_,smats_,fvec_,cmats_,dmats_}]:=
With[{seqns=Length[smats],scols=Length[smats[[1]]],
eqns=Length[cmats[[1]]]},
With[{leads=(Length[smats[[1]]]/eqns) - 1-lags},
With[{riffed=riffle[cmats[[Range[-lags,-1]]],
dmats[[Range[-lags,-1]]],smats,fvec]},
With[{
hlu=
  LUDecomposition[SubMatrix[riffed[[1]],{1,1},{seqns,seqns}]]
},
{Append[cmats,LUBackSubstitution[hlu,SubMatrix[riffed[[1]],{1,seqns+1},
  {seqns,eqns}]]],
Append[dmats,LUBackSubstitution[hlu,riffed[[2]]]]}]]]]





(*last cmat better be all zeros or don't have enough equations to determine
delta y*)
backSub[cmats_,dmats_]:=
With[{rc=Rest[Reverse[cmats]],rd=Rest[Reverse[dmats]]},
With[{ytail=dmats[[{-1}]]},
Reverse[oneStepBack[{ytail,rc,rd}][[1]]]]]


oneStepBack[{yvec_,rcmats_,rdvecs_}]:=
If[rcmats==={},{yvec,rcmats,rdvecs},
oneStepBack[{Append[yvec,
  rdvecs[[1]]-rcmats[[1]] . yvec[[-1,Range[-Length[rcmats[[1,1]]],-1]]]],
  Rest[rcmats],Rest[rdvecs]}]]



modelPrep[eqns_List]:=
With[{vbls=eqnVars[eqns],ll=lagLead[eqns]},
With[{argVal=Flatten[Table[Map[#[t+i]&,vbls],{i,-ll[[1]],ll[[2]]}]]},
With[{theSubs=Thread[argVal->Table[Unique[],{(ll[[2]]+ll[[1]]+1)*Length[vbls]}]]},
With[{drvsModel=Map[Function[xx,Map[D[xx,#]&,argVal]],eqns]},
With[{aModDrvFunc = Function @@ ({argVal,drvsModel}/.theSubs)},
{aModDrvFunc}]]]]]


(*{argVal/.theSubs,mod[[1]]/.theSubs}]]]]},
With[{aModFunc = Apply[Function, *)

nxtGuess[nlag_,theFunc_,theDrvFunc_,termConstr_,fp_,guess_]:=
With[{neq=Length[theDrvFunc[[2]]]},
With[{nlead=(Length[theDrvFunc[[2,1]]]/neq)-nlag-1},
Module[{nxlstC,nxlstD,lstC,lstD},
With[{appDim=(nlag+nlead+1)*neq,
theZeroMatsC=Table[ZeroMatrix[neq,neq],{nlag}],
theZeroMatsD=Table[ZeroMatrix[neq,1],{nlag}]
},
With[{theArgs=Partition[guess,appDim,neq]},
With[{theRes=Map[Apply[theFunc,#]& , theArgs]},
{nxlstC,nxlstD}=Fold[Function[{x,y},nxtCDmats[{nlag,
Apply[theDrvFunc,y],
Apply[theFunc,y],x[[1]],x[[2]]}]],
{theZeroMatsC,theZeroMatsD},theArgs];
{lstC,lstD}=nxtCDmats[{nlag,
BlockMatrix[{{termConstr,ZeroMatrix[Length[termConstr],neq]}}],
termConstr . (Transpose[{guess[[Range[-neq*(nlag+nlead),-1]]]}]- 
Transpose[{fp[[Range[-neq*(nlag+nlead),-1]]]}]),nxlstC,nxlstD}];
bs=backSub[lstC,lstD];
guess-Flatten[bs]]]]]]]



nxtFpGuess[nlag_,theFunc_,theDrvFunc_,guess_]:=
With[{neq=Length[theDrvFunc[[2]]]},
With[{nlead=(Length[theDrvFunc[[2,1]]]/neq)-nlag-1},
Module[{nxlstC,nxlstD,lstC,lstD},
With[{appDim=(nlag+nlead+1)*neq,
theIdentMatsC=Table[-IdentityMatrix[neq],{nlag}],
theZeroMatsD=Table[ZeroMatrix[neq,1],{nlag}]
},
With[{theArgs=Partition[guess,appDim,neq]},
With[{theRes=Map[Apply[theFunc,#]& , theArgs]},
{nxlstC,nxlstD}=Fold[Function[{x,y},nxtCDmats[{nlag,
Apply[theDrvFunc,y],
Apply[theFunc,y],x[[1]],x[[2]]}]],
{theIdentMatsC,theZeroMatsD},theArgs];
With[{nstFunc=Function[x,
nxtCDmats[{nlag,
BlockMatrix[{{ZeroMatrix[neq,neq*(nlag-1)],
IdentityMatrix[neq],-IdentityMatrix[neq],ZeroMatrix[neq,neq*(nlead)]}}],
ZeroMatrix[neq,1],x[[1]],x[[2]]}]]},
{lstC,lstD}=Nest[nstFunc,{nxlstC,nxlstD},nlead]];
bs=backSub[lstC,lstD];
guess-Flatten[bs]]]]]]]



denseToSparseRow[{{jNow_Integer,jArray_List,aArray_List},row_List}]:=
If[row === {},{{jNow,jArray,aArray},row},
If[row[[1]] === 0,denseToSparseRow[{{jNow+1,jArray,aArray},Rest[row]}],
denseToSparseRow[{{jNow+1,Append[jArray,jNow],Append[aArray,row[[1]]]},Rest[row]}]]]
denseToSparseMat[mat_List]:=
With[{firstCut=Map[denseToSparseRow[Append[{{1,{},{}}},#]]&,mat]},
With[{jStuff=Map[#[[1,2]]&,firstCut],
aStuff=Map[#[[1,3]]&,firstCut]},
With[{lens=Map[Length,jStuff]},
{Flatten[aStuff],Flatten[jStuff],FoldList[Plus,1,lens]}]]]


End[]
EndPackage[]
