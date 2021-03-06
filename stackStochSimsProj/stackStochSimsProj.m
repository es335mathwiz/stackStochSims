(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 1, 2016 *)

BeginPackage["stackStochSimsProj`", {"JLink`","Stack`"}]
(* Exported symbols added here with SymbolName::usage *) 
$toggleDirLoc::usage="loc of toggle dir"
Begin["Private`"]
(* Implementation of the package *)
SetDirectory["g:/git/stackStochSims"]


$toggleDirLoc=FileNameJoin[{Directory[],"/"},OperatingSystem->$OperatingSystem];
$toGit=FileNameJoin[{Directory[],"../"},OperatingSystem->$OperatingSystem];
Print["togit=",$toGit];
$fmtPath=FileNameJoin[{$toGit,"paperProduction/FormatOptimize/"}];
(*$Path=PrependTo[$Path,FileNameJoin[{$toGit,"experFRBUSMPS"}];*)
$Path=PrependTo[$Path,FileNameJoin[{$toGit,"mathAMA/AMAModel"}]];
$symPath=FileNameJoin[{$toGit,"mathAMA/SymbolicAMA/SymbolicAMA/"}];
$numPath=FileNameJoin[{$toGit,"mathAMA/NumericAMA/NumericAMA/"}];
$modPath=FileNameJoin[{$toGit,"mathAMA/AMAModel/"}];
$proPath=FileNameJoin[{$toGit,"ProtectedSymbols/"}];
$accPath=FileNameJoin[{$toGit,"AccelerateAMA/AccelerateAMA/"}];
$stackStochPath=FileNameJoin[{$toGit,"stackStochSims/"}];
$Path = 
Join[$Path, 
{
$modPath,
$symPath,
$numPath,
$accPath,
$proPath,
$fmtPath,
$stackStochPath
(*,
$amaSer,
$smolPath*)}];

$toSoftware=FileNameJoin[{$toGit, "../softwareInstalls/"}];
$reposLoc=FileNameJoin[{$toGit,"../.m2/repository"}]
(*
Needs["AMAModel`"]
Needs["AccelerateAMA`"]
Needs["Stack`"]
Needs["Stoch`"]
*)

Needs["JLink`"]
InstallJava[];

AddToClassPath[FileNameJoin[{$reposLoc,"gov/frb/ma/msu/dynareAntlr/1.0/dynareAntlr-1.0.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"org/antlr/antlr/3.5.2/antlr-3.5.2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"org/antlr/antlr-runtime/3.5.2/antlr-runtime-3.5.2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"SAXON-HE/saxon9-xqj/9-7-0-2/saxon9-xqj-9-7-0-2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"SAXON-HE/saxon9he/9-7-0-2/saxon9he-9-7-0-2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"xalan-j_2_7_1/xalan/2-7-1/xalan-2-7-1.jar"}]];

Print[FileNameJoin[{$reposLoc,"xalan-j_2_7_1/xalan/2-7-1/xalan-2-7-1.jar"}]]
Print["setup in stackStochSimsProj"]

End[]

EndPackage[]

