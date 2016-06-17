(* Wolfram Language Package *)

BeginPackage["MQXMLToDynareMod`",{"JLink`","Stack`"}]

(* Exported symbols added here with SymbolName::usage *)  
cnstrctDModel::usage="cnstrctDModel[inFName_String,outFName_String]"
getVarNames::usage="getVarNames[inFile_String]"
Begin["`Private`"] (* Begin Private Context *) 


$frbLocDesktop="g:/.m2/repository/";
$frbLoc="/msu/home/m1gsa00/.m2/repository/";
$garyMacLoc="/Users/garyanderson/.m2/repository/";
$reposLoc=
Which[
FileExistsQ[$frbLoc],$frbLoc,
FileExistsQ[$frbLocDesktop],$frbLocDesktop,
FileExistsQ[$garyMacLoc],$garyMacLoc
];



theNextString[aSeq_?JavaObjectQ]:=
With[{prime=aSeq[next[]]},If[prime,aSeq[getItemAsString[Null]],"$noMore"]]

sequenceToListString[aSeq_?JavaObjectQ]:=
Drop[Drop[FixedPointList[theNextString[aSeq]&,xx],1],{-2,-1}]


theNextNumber[aSeq_?JavaObjectQ]:=
With[{prime=aSeq[next[]]},If[prime,aSeq[getDouble[]],"$noMore"]]

sequenceToListNumber[aSeq_?JavaObjectQ]:=
Drop[Drop[FixedPointList[theNextNumber[aSeq]&,xx],1],{-2,-1}]




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




elemsToSubs[xml_]:=
Cases[xml,XMLElement["coeffInfo", {"theName" -> tn_String, 
        "theValue" -> tv_String}, {}]->(ToExpression[tn]->ToExpression[tv]),Infinity]


elemsToAssns[xml_]:=
Cases[xml,XMLElement["coeffInfo", {"theName" -> tn_String, 
        "theValue" -> tv_String}, {}]:>{tn,(tn<>"="<>tv)},Infinity]



doModStr[allVars_List,coeffs_List,eqns_List,vcSubs_List,inFName_String,outFName_String]:=
With[{modStr=StringReplace[
"var " <> (StringJoin @@ Riffle[allVars," "]) <> ";\n" <>
ridParens["parameters " <> (StringJoin @@ Riffle[First/@coeffs," "]) <> 
";\n"] <>
ridParens[(StringJoin @@ Riffle[Last/@coeffs,";\n"]) <> ";\n"] <>
"model;\n"<> (StringJoin @@ Riffle[eqns,";\n"]) <>";\n end;\n",vcSubs]},
WriteString[outFName,modStr]]


End[] (* End Private Context *)

EndPackage[]