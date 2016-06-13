(* Wolfram Language package *)
(*should be in run from doToggle directory,
subdirectory of the directory git containing 
the required directories*)

Needs["AccelerateAMA`"]
Print["code should go to package"]
getCoeffs[inFile_String]:=
Module[{ds,conn,exp,result,sTemplate=StringTemplate[
"<modCoefficients>{
let $mod:=doc('`1`')
for $var in $mod//model/variable
return 
if ($var/mce_equation/coeff)
then (
for $coeffMCE in $var/mce_equation/coeff 
let $nmMCE := $coeffMCE/cf_name/text()
let $vlMCE := $coeffMCE/cf_value/number()
return  <coeffInfo theName=\"{$nmMCE}\" theValue=\"{$vlMCE}\"/>)
else(
for $coeff in $var/standard_equation/coeff 
let $nm := $coeff/cf_name/text()
let $vl := $coeff/cf_value/number()
return  <coeffInfo theName=\"{$nm}\" theValue=\"{$vl}\"/>)
}</modCoefficients>"]},
ds=JavaNew["com.saxonica.xqj.SaxonXQDataSource"];
conn = ds[getConnection[]];
exp = conn[prepareExpression[
sTemplate[inFile]]];
result = exp[executeQuery[]];
elemsToAssns[ImportString[
sequenceToListString[result][[1]],"XML"]
]]


elemsToSubs[xml_]:=
Cases[xml,XMLElement["coeffInfo", {"theName" -> tn_String, 
        "theValue" -> tv_String}, {}]->(ToExpression[tn]->ToExpression[tv]),Infinity]


elemsToAssns[xml_]:=
Cases[xml,XMLElement["coeffInfo", {"theName" -> tn_String, 
        "theValue" -> tv_String}, {}]:>{tn,(tn<>"="<>tv)},Infinity]



theNextString[aSeq_?JavaObjectQ]:=
With[{prime=aSeq[next[]]},If[prime,aSeq[getItemAsString[Null]],"$noMore"]]

sequenceToListString[aSeq_?JavaObjectQ]:=
Drop[Drop[FixedPointList[theNextString[aSeq]&,xx],1],{-2,-1}]


theNextNumber[aSeq_?JavaObjectQ]:=
With[{prime=aSeq[next[]]},If[prime,aSeq[getDouble[]],"$noMore"]]

sequenceToListNumber[aSeq_?JavaObjectQ]:=
Drop[Drop[FixedPointList[theNextNumber[aSeq]&,xx],1],{-2,-1}]


getVarNames[inFile_String]:=
Module[{ds,conn,exp,result,sTemplate=StringTemplate[
"let $mod:=doc('`1`')
for $var in $mod//model/variable/name/text() return $var"]},
ds=JavaNew["com.saxonica.xqj.SaxonXQDataSource"];
conn = ds[getConnection[]];
exp = conn[prepareExpression[
sTemplate[inFile]]];
result = exp[executeQuery[]];
sequenceToListString[result]
]

nameSubs[aStr_String]:=(ridParens[aStr]->ridUSSubs[ridParens[aStr]])

nameSubsWP[aStr_String]:=(aStr->ridUSSubs[ridParens[aStr]])


ridParens[str_String]:=StringReplace[str,{"("->"_",")"->""}]

ridUSSubs[str_String]:=StringReplace[
StringReplace[str,{"("->"_",")"->""}],"_"->"US"]





getEVEqns[inFile_String]:=
Module[{ds,conn,exp,result,sTemplate=StringTemplate[
"{let $mod:=doc('`1`')
for $eqn in $mod//model/variable/mce_equation/eviews_equation/text() return $eqn}"]},
ds=JavaNew["com.saxonica.xqj.SaxonXQDataSource"];
conn = ds[getConnection[]];
exp = conn[prepareExpression[
sTemplate[inFile]]];
result = exp[executeQuery[]];
sequenceToListString[result]
]


getDynareEqns[inFile_String]:=
Module[{ds,conn,exp,result,sTemplate=StringTemplate[
"let $mod:=doc('`1`')
for $var in $mod//model/variable 
return (if ($var/mce_equation) then $var/mce_equation/dynare_equation/text() else if ($var/standard_equation/dynare_equation) then $var/standard_equation/dynare_equation/text() else $var/name/text())
"]},
ds=JavaNew["com.saxonica.xqj.SaxonXQDataSource"];
conn = ds[getConnection[]];
exp = conn[prepareExpression[
sTemplate[inFile]]];
result = exp[executeQuery[]];
sequenceToListString[result]
]



$frbLocDesktop="g:/.m2/repository/";
$frbLoc="/msu/home/m1gsa00/.m2/repository/";
$garyMacLoc="/Users/garyanderson/.m2/repository/";
$reposLoc=
Which[
FileExistsQ[$frbLoc],$frbLoc,
FileExistsQ[$frbLocDesktop],$frbLocDesktop,
FileExistsQ[$garyMacLoc],$garyMacLoc
];



cnstrctDModel[inFName_String,outFName_String]:=
With[{eqns=getDynareEqns[inFName],
vars=getVarNames[inFName],
coeffs=getCoeffs[inFName]},
With[{addFctrs=#<>"_aerr"&/@vars},
With[{allVars=Join[addFctrs,vars]},
With[{vcSubs=
DeleteCases[Join[(nameSubs/@allVars),
(nameSubsWP/@(First/@coeffs)),
(nameSubs/@(First/@coeffs))],xx_->xx_]},
{eqns,vars,coeffs,doModStr[allVars,coeffs,eqns,vcSubs,inFName,outFName]}]]]]





doModStr[allVars_List,coeffs_List,eqns_List,vcSubs_List,inFName_String,outFName_String]:=
With[{modStr=StringReplace[
"var " <> (StringJoin @@ Riffle[allVars," "]) <> ";\n" <>
ridParens["parameters " <> (StringJoin @@ Riffle[First/@coeffs," "]) <> 
";\n"] <>
ridParens[(StringJoin @@ Riffle[Last/@coeffs,";\n"]) <> ";\n"] <>
"model;\n"<> (StringJoin @@ Riffle[eqns,";\n"]) <>";\n end;\n",vcSubs]},
WriteString[outFName,modStr]]


Print["executing code for rbc.in.xml example"]
xmlFileRBC=StringReplace[
"file:///"<>FileNameJoin[{Directory[],"/rbc.in.xml"},
OperatingSystem->"Unix"],"\\"->"/"]


caRBC=getCoeffs[xmlFileRBC];




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


(*
betterSSSubs=MapThread[(#1[[2]]->#2[[2]])&,{Sort[ssVarsRBC],
Sort[Append[ssSolnSubsPF,aDummy->0]]//N}]
*)







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



Print["just before usingparamSubsrbc"]

eqnsRBCSSSubbed=(eqnsRBC/.paramSubsRBC)/.theSSVarsSubs/.headStart/.zapAerr






eqnsRBCSubbed=eqnsRBC/.paramSubsRBC;


Print["just before getting hmatsym"]

{hmatTimeRBC,hmatRBC}=Timing[(equationsToMatrix[eqnsRBCSubbed/.((#->0)&/@varsAerrRBC),varsNoAerrRBC]/.makeSSValSubs[varsNoAerrRBC])];

Print["got hmatRBC"]

{{zfRBC,ig},ig,ig,{evlsRBC,evcsRBC},qmatRBC,bmatRBC,ig,ig}=numericAMA[hmatRBC/.ssSolnSubsPF,1,1];
{rbcF,rbcDF}=modelPrep[eqnsRBCSubbed/.zapAerr];


fpCondMat = 
 blockMatrix[{{0*
     IdentityMatrix[4], -IdentityMatrix[4]}}]; 
fpTermVal = Last /@ Join[ssSolnSubsPF,ssSolnSubsPF]
fpVal = Last /@ ssSolnSubsPF

$pathLen=200;

initX = Table[0.02*Random[], {4}] + fpVal;
initPath =  Table[0.001*Random[], {4*$pathLen}] + 
Flatten[Table[fpVal, {$pathLen}]];


rbcPath[len_Integer] :=
 With[{bip = initPath[[Range[4*len]]]}, 
  Nest[nxtGuess[1, rbcF, rbcDF, fpCondMat, fpTermVal, #] &, 
    Join[initX, bip], 20][[4 +
     Range[4]]]]

rbcPathAMA[len_Integer] :=
 With[{bip = initPath[[Range[4*len]]]}, 
  Nest[nxtGuess[1, rbcF, rbcDF, qmatRBC // N, 
      fpTermVal, #] &, Join[initX, bip], 20][[4 +
     Range[4]]]]

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
stochSim[t0,tf,reps,model,horizon,expType]

stochSim[2, 2, 1, rbcTestModel, 1, t] // Chop // TableForm]

*)


