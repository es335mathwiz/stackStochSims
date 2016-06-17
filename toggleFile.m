(* Wolfram Language package *)
(*should be in run from stackStochSims directory,
subdirectory of the directory git containing 
the required directories*)
Needs["MQXMLToDynareMod`"]
Needs["AccelerateAMA`"]


(*transforming mqxml model into dynare mod*)
Print["executing code for rbc.in.xml example"]
xmlFileRBC=StringReplace[
"file:///"<>FileNameJoin[{Directory[],"/rbc.in.xml"},
OperatingSystem->"Unix"],"\\"->"/"]

Print["take care with names since\n cnstrctDModel appends to xxx.mod and\n parsemod overwrites xxx.xml"]
Print["deleting rbc.mod in",Directory[],FileExistsQ["rbc.mod"],FindFile["rbc.mod"]]
DeleteFile["rbc.mod"]
Print["done deleting rbc.mod in",Directory[],FileExistsQ["rbc.mod"],FindFile["rbc.mod"]]
{rbcEqns,rbcVars,rbcCoeffs,ig}=baRBC=cnstrctDModel[xmlFileRBC,"rbc.mod"];



{varsRBC,ig,paramsRBC,ig,{ig,eqnsRBC},notSubsRBC,ig}=parseMod[$toggleDirLoc,"rbc",$toggleDirLoc]

varsNoAerr=ToExpression/@getVarNames[xmlFileNow];



varsNoAerrRBC=ToExpression/@getVarNames[xmlFileRBC];
ssVarsRBC=makeSSValSubs[varsNoAerrRBC];



varsAerrRBC=Complement[varsRBC,varsNoAerrRBC];


Print["making paramsubsrbc"]

paramSubsRBC=(#[[1]]->#[[2]])&/@paramsRBC;

headStart={thetaSSVal->1,aDummySSVal->0}
zapAerr=Join[((#->0)&/@varsAerrRBC),((#[t]->0)&/@varsAerrRBC)]
theSSVarsSubs=makeSSValSubs[varsNoAerrRBC];

Print["making sssolnsubs"]
If[Length[ssSolnSubsPF]===0,
Print["computing steady state subs"];
thSubsPF=thetaSSVal->1;aDSubsPF=aDummyVal->0;
nxtK[lastK_,thNowVal_]:=((yUSccUS1*yUSccUS2))*thNowVal*lastK^(yUSkkUS1)/.paramSubsRBC;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(yUSkkUS1)/.paramSubsRBC;
Identity[kSSSubPF=Flatten[Solve[nxtK[kkSSVal,thetaSSVal/.thSubsPF]==kkSSVal,kkSSVal]][[-1]]];
Identity[cSSSubPF=ccSSVal->(yNow[kkSSVal/.kSSSubPF,thetaSSVal/.thSubsPF]-kkSSVal/.kSSSubPF)];
Identity[ssSolnSubsPF=Sort[Flatten[{aDSubsPF=aDummyVal->0,thSubsPF,kSSSubPF,cSSSubPF}]]];
Print["done computing steady state subs"];
]

fpVal = Last /@ ssSolnSubsPF

Print["just before usingparamSubsrbc"]

eqnsRBCSSSubbed=(eqnsRBC/.paramSubsRBC)/.theSSVarsSubs/.headStart/.zapAerr






eqnsRBCSubbed=eqnsRBC/.paramSubsRBC;


$pathLen=200;

initX = Table[0.02*Random[], {4}] + fpVal;
initPath =  Table[0.001*Random[], {4*$pathLen}] + 
Flatten[Table[fpVal, {$pathLen}]];

rbcTestModel =.;
lags[rbcTestModel] = 1;
leads[rbcTestModel] = 1;
eqns[rbcTestModel] = 4;


shocks[rbcTestModel] = Table[Random[],{500},{4}];

xData[rbcTestModel] = 
Table[Table[0.001*Random[], {4}] +fpVal,{500}];


func[rbcTestModel] = rbcF;


drvFunc[rbcTestModel] = rbcDF;
qMat[rbcTestModel] = qmatRBC;
fp[rbcTestModel] = Last /@ Join[ssSolnSubsPF,ssSolnSubsPF,ssSolnSubsPF];





(*

Print["just before getting hmatsym"]

{hmatTimeRBC,hmatRBC}=Timing[(equationsToMatrix[eqnsRBCSubbed/.((#->0)&/@varsAerrRBC),varsNoAerrRBC]/.makeSSValSubs[varsNoAerrRBC])];

Print["got hmatRBC"]

{{zfRBC,ig},ig,ig,{evlsRBC,evcsRBC},qmatRBC,bmatRBC,ig,ig}=numericAMA[hmatRBC/.ssSolnSubsPF,1,1];
{rbcF,rbcDF}=modelPrep[eqnsRBCSubbed/.zapAerr];


fpCondMat = 
 blockMatrix[{{0*
     IdentityMatrix[4], -IdentityMatrix[4]}}]; 
fpTermVal = Last /@ Join[ssSolnSubsPF,ssSolnSubsPF]

*)
