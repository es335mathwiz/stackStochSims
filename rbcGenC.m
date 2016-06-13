(* Wolfram Language package *)

Needs["MmaModelToC`"]
rbcForC=eqnsRBCSubbed/.zapAerr;

modelFunctionName[rbcForC]="rbcExample";
modelInfo[rbcForC]=  "rbc example model";

modelDataInfo[rbcForC]="made up data from normal dist";
modelData[rbcForC]=xData[rbcTestModel];



modelShockInfo[rbcForC]=
modelShocks[rbcForC]=shocks[rbcTestModel];


modelDefaultParameters[rbcForC]={};


modelFpGuess[rbcForC]=fpVal;
(*
Private`doSplice[rbcForC,"rbcTryC"]
*)
