(* Wolfram Language Package *)

BeginPackage["Stoch`", { "Stack`","SymbolicAMA`","ProtectedSymbols`"}]
(* Exported symbols added here with SymbolName::usage *)  
lags::usage="lags[model]";
leads::usage="leads[model]";
eqns::usage="eqns[model]";
func::usage="func[model]";
drvFunc::usage="drvFunc[model]";
xData::usage="xData[model]";
shocks::usage="shocks[model]";
stochSim::usage="stochSim[t0Index_Integer,tfIndex_Integer,replications_Integer,model_Symbol,horizon_Integer,expType_Symbol]"
tMinusOne::usage="an expectation type"
fp::usage="model fixed point"
qMat::usage="qmat of model"
aimType2Terror::usage="aimType2Terror[shock_,model_, horizon_, init_]"
aimType2::usage="aimType2[model_, horizon_, init_]"
compXEtm1::usage="compXEtm1[model,expectations,shock]"
Begin["Private`"] (* Begin Private Context *) 
lags[model_]:=-First[lagsLeads[model]];
leads[model_]:=Last[lagsLeads[model]];
eqns[model_]:=Length[model];
func[model_]:=modelPrep[duh[model]][[1]];
drvFunc[model_]:=modelPrep[duh[model]][[2]];
xData[model_]:=modelData[model]
shocks[model_]:=modelShocks[model]

stochSim[t0Index_Integer,tfIndex_Integer,
        replications_Integer,
                model_Symbol,horizon_Integer,expType_Symbol]:=
With[{mlags=lags[model]},
With[{laggedDataValues=xData[model][[t0Index+Range[-mlags,-1]]]},
With[{shockSeqList=
generateDraws[t0Index,tfIndex,replications,Length[shocks[model]]],
iterFunc=(generatePathX[{model,horizon,expType,laggedDataValues},#])&},
Map[iterFunc,shockSeqList]
]]]/;(t0Index>lags[model] && tfIndex>=t0Index && 
   horizon>0 && replications>0&&((expType == tMinusOne)||(expType == t)))


generateDraws[t0Index_Integer,tfIndex_Integer,
                replications_Integer,shocksAvailable_Integer]:=
        Table[RandomVariate[
        DiscreteUniformDistribution[{1,shocksAvailable}]],
        {replications},{tfIndex-t0Index+1}]




generatePathX[{model_Symbol,horizon_Integer,expType_Symbol,xInit_List},
                                shockSeq_List]:=
                With[{(*shock=shocks[model],*)
        nxtFunc=If[expType === tMinusOne,generateNextXTMinusOne,
        generateNextXT]},
                {Fold[nxtFunc,
          {model,horizon,expType,xInit},
          shockSeq][[-1]],shockSeq}]




generateNextXTMinusOne[
{model_Symbol,horizon_Integer,expType_Symbol,xPath_List},
        shockIndex_Integer]:=
With[{shock=shocks[model][[shockIndex]],
 numEq=Length[func[model][[2]]],
 mleads=leads[model],
 mlags=lags[model]},
With[{expectations=
(aimType2[model,horizon,
   Flatten[xPath][[
       Range[-numEq*mlags,-1]]]][[
             -1,Range[numEq*(mlags+mleads+1)]]])},
{model,horizon,expType,Append[xPath,
  compXEtm1[model,expectations,shock][[numEq*mlags+Range[numEq]]]]}]]


aimType2[model_Symbol,horizon_,init_]:=FixedPointList[
nxtGuess[lags[model],
 func[model],
 drvFunc[model],
 qMat[model],
 fp[model],#]&,
 Join[init,Flatten[Table[fp[model][[Range[eqns[model]]]],{(horizon+leads[model])}]]],
 SameTest->((((*(*Print[{#1,#2}];*)*)Max[Abs[#1-#2]]<10^(-15)))&)]




compXEtm1[model_Symbol,expectations_List,shock_List]:=
With[{deFunc=func[model]},
With[{mlags=lags[model],mleads=leads[model],
  numEq=Length[func[model][[2]]],
  newFunc=
    Apply[Function,{deFunc[[1]],deFunc[[2]]-shock}]},
With[{},
 FixedPoint[
  nxtGuess[lags[model],
  newFunc,
  drvFunc[model],
  blockMatrix[{{zeroMatrix[numEq*mleads,numEq*(mlags)],IdentityMatrix[numEq*mleads]}}],
  expectations,#]&,
  expectations,
  SameTest->(((*(*Print[{#1,#2}];*)*)Max[Abs[#1-#2]]<10^(-15))&)]]]]




generateNextXT[
{model_Symbol,horizon_Integer,expType_Symbol,xPath_List},
        shockIndex_Integer]:=
With[{shock=shocks[model][[shockIndex]],
 numEq=Length[func[model][[2]]],
 mlags=lags[model]},
With[{},
{model,horizon,expType,Append[xPath,aimType2Terror[shock,model,horizon,
   Flatten[xPath][[
       Range[-numEq*mlags,-1]]]][[
             -1,numEq*mlags+Range[numEq]]]]}]]





nxtGuessTerror[shock_List,nlag_Integer,
theFunc_Function,theDrvFunc_Function,
termConstr_List,fp_List,guess_List]:=
With[{neq=Length[theDrvFunc[[2]]]},
With[{nlead=(Length[theDrvFunc[[2,1]]]/neq)-nlag-1},
Module[{nxlstC,nxlstD,lstC,lstD},
With[{appDim=(nlag+nlead+1)*neq,
theZeroMatsC=Table[zeroMatrix[neq,neq],{nlag}],
theZeroMatsD=Table[zeroMatrix[neq,1],{nlag}]
},
With[{theArgs=Partition[guess,appDim,neq]},
With[{},
With[{newFunc=
    Apply[Function,{theFunc[[1]],theFunc[[2]]-shock}]},
With[{prime=nxtCDmats[{nlag,
Apply[theDrvFunc,theArgs[[1]]],
Apply[newFunc,theArgs[[1]]],
theZeroMatsC,theZeroMatsD}]},(*Print["prime=",prime,"applic=",Apply[newFunc,theArgs[[1]]],"theArgs[[1]]=",theArgs[[1]],"shock=",shock,"theArgs=",(*InputForm[Rationalize[#,1/10000000000000000]]& MapThingy*)theArgs,"theRes",theRes];*)
{nxlstC,nxlstD}=Fold[Function[{x,y},nxtCDmats[{nlag,
Apply[theDrvFunc,y],(*Print["applying theFunc=",Apply[theFunc,y],y];*)
Apply[theFunc,y],x[[1]],x[[2]]}]],
prime(*{theZeroMatsC,theZeroMatsD}*),Drop[theArgs,1]];
{lstC,lstD}=nxtCDmats[{nlag,
blockMatrix[{{termConstr,zeroMatrix[neq]}}],
termConstr . (Transpose[{guess[[Range[-neq*(nlag+nlead),-1]]]}]- 
Transpose[{fp[[Range[-neq*(nlag+nlead),-1]]]}]),nxlstC,nxlstD}];
bs=backSub[lstC,lstD];
guess-Flatten[bs]]]]]]]]]



aimType2Terror[shock_,model_, horizon_, init_] := FixedPointList[
    nxtGuessTerror[shock,lags[model], func[model], drvFunc[model], qMat[model], 
      fp[model], #1] & , Join[init, Flatten[Table[fp[model][[Range[eqns[model]]]], 
       {horizon+1 + leads[model]}]]], SameTest -> 
     (((*Print[{#1,#2}];*)Max[Abs[#1 - #2]] < 10^(-15) )& )]




aimType3[model_,initHoriz_,init_]:=
With[{numEq=Length[func[model][[2]]]},
FixedPointList[
{#[[1]]+1,aimType2[model,#[[1]]+1,init][[-1,numEq+Range[numEq]]]}&,{0,Table[0,{numEq}]},
SameTest->(((*Print[{#1,#2}];*)
Max[Abs[#1[[2]]-#2[[2]]]]<10^(-15))&)]];



tMinusOneErrorFunc[model_,expectations_List]:=
With[{mlags=lags[model],mleads=leads[model],
  numEq=Length[func[model][[2]]]},
With[{xAtTZero=Table[Unique[],{numEq}]},
With[{argList=Join[expectations[[Range[numEq*mlags]]],xAtTZero,
  expectations[[numEq*(mlags+1)+Range[numEq*mleads]]]]},
Apply[Function,{xAtTZero,Apply[func[model],argList]}]]]]


End[] (* End Private Context *)

EndPackage[]
