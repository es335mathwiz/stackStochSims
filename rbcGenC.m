(* Wolfram Language package *)

Needs["MmaModelToC`"]

(*object oriented programming style*)
(*a model need a list of equations and 8 methods associated with 
the list of equations producing components needed to generate the c code*)

rbcForC=eqnsRBCSubbed/.zapAerr;

modelFunctionName[rbcForC]="rbcExample";

modelInfo[rbcForC]=  "rbc example model";

modelDataInfo[rbcForC]="made up data from normal dist";

modelData[rbcForC]=xData[rbcTestModel];

modelShockInfo[rbcForC]="some shocks info"

modelShocks[rbcForC]=shocks[rbcTestModel];

modelDefaultParameters[rbcForC]={};

modelFpGuess[rbcForC]=fpVal;


generateCCode[ rbcForC,"muddy"]
