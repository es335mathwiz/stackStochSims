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


denseColToSparseMat::usage="denseColToSparseMat[vec_List]"

Begin["Private`"]


denseColToSparseMat[vec_List]:=
{vec,Table[1,{Length[vec]}],Range[Length[vec]+1]}



riffle[cmatList_,dmatList_,smat_,fvec_]:=
If[cmatList === {},{smat,fvec},
With[{cmatNow=cmatList[[1]],cmatRest=Rest[cmatList],
dmatNow=dmatList[[1]],dmatRest=Rest[dmatList]},
With[{seqns=Length[smat],
eqns=Length[cmatNow],scols=Length[smat[[1]]],ccols=Length[cmatNow[[1]]]},
With[{expand=blockMatrix[{{cmatList[[1]],
      zeroMatrix[eqns,scols-ccols-eqns]}}],
smatSub=subMatrix[smat,{1,1},{seqns,eqns}] },
Apply[riffle,{cmatRest,dmatRest,subMatrix[smat,{1,eqns+1},{seqns,scols-eqns}] -
  smatSub. expand,fvec - smatSub . dmatNow}]]]]]



eqnVars[eqns_]:=Union[Cases[
eqns,x_[t]->x,Infinity],Cases[eqns,x_[t+_]->x,Infinity]]



lagLead[eqns_]:=
With[{lagLead=Union[Cases[eqns,_[t]->0,{Infinity}],Cases[eqns,_[t+x_]->x,Infinity]]},
With[{maxLag=-Min[lagLead],maxLead=Max[lagLead]},
{maxLag,maxLead}]]



nxtCDmats[{lags_,smats_,fvec_,cmats_,dmats_}]:=
With[{seqns=Length[smats],
eqns=Length[cmats[[1]]]},
With[{},
With[{riffed=riffle[cmats[[Range[-lags,-1]]],
dmats[[Range[-lags,-1]]],smats,fvec]},
With[{
hlu=
  LUDecomposition[subMatrix[riffed[[1]],{1,1},{seqns,seqns}]],
  theSubMat=subMatrix[riffed[[1]],{1,seqns+1},
  {seqns,eqns}]
},
{Append[cmats,LUBackSubstitution[hlu,theSubMat]],
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
With[{aModFunc=Function @@ ({argVal,eqns}/.theSubs)},
	With[{
	aModDrvFunc = Function @@ ({argVal,drvsModel}/.theSubs)},
{aModFunc,aModDrvFunc}]]]]]]


(*{argVal/.theSubs,mod[[1]]/.theSubs}]]]]},
With[{aModFunc = Apply[Function, *)

nxtGuess[nlag_,theFunc_,theDrvFunc_,termConstr_,fp_,guess_]:=
With[{neq=Length[theDrvFunc[[2]]]},
With[{nlead=(Length[theDrvFunc[[2,1]]]/neq)-nlag-1},
Module[{nxlstC,nxlstD,lstC,lstD},
With[{appDim=(nlag+nlead+1)*neq,
theZeroMatsC=Table[zeroMatrix[neq,neq],{nlag}],
theZeroMatsD=Table[zeroMatrix[neq,1],{nlag}]
},
With[{theArgs=Partition[guess,appDim,neq]},
With[{},
{nxlstC,nxlstD}=Fold[Function[{x,y},nxtCDmats[{nlag,
Apply[theDrvFunc,y],
Apply[theFunc,y],x[[1]],x[[2]]}]],
{theZeroMatsC,theZeroMatsD},theArgs];
{lstC,lstD}=nxtCDmats[{nlag,
blockMatrix[{{termConstr,zeroMatrix[Length[termConstr],neq]}}],
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
theZeroMatsD=Table[zeroMatrix[neq,1],{nlag}]
},
With[{theArgs=Partition[guess,appDim,neq]},
With[{},
{nxlstC,nxlstD}=Fold[Function[{x,y},nxtCDmats[{nlag,
Apply[theDrvFunc,y],
Apply[theFunc,y],x[[1]],x[[2]]}]],
{theIdentMatsC,theZeroMatsD},theArgs];
With[{nstFunc=Function[x,
nxtCDmats[{nlag,
blockMatrix[{{zeroMatrix[neq,neq*(nlag-1)],
IdentityMatrix[neq],-IdentityMatrix[neq],zeroMatrix[neq,neq*(nlead)]}}],
zeroMatrix[neq,1],x[[1]],x[[2]]}]]},
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
