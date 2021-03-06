(*
cd ~;mkdir someDir;cd someDir
to make sparseAMA stackC and stochSims code
git clone https://github.com/es335mathwiz/sparseAMA.git
git clone https://github.com/es335mathwiz/CStochSims.git
cd sparseAMA
if on msulx1 since java requests too much space for jvm use 
export MAVEN_OPTS="-Xms64m -Xmx64m"
mvn clean install
cd ../CStochSims
make -f makeStochTry debStochRun
make -f makeStochTry stochRun

to make code generation and toggle code return to someDir
cd ..
git clone https://github.com/es335mathwiz/stackStochSims.git
git clone https://github.com/es335mathwiz/AccelerateAMA.git
git clone https://github.com/es335mathwiz/ProtectedSymbols.git
git clone https://github.com/es335mathwiz/mathAMA.git
cd stackStochSims
math  run version 10
Get["fixSplice.m"]
Quit

gmake -f muddyMakefile debrunmuddy
export LD_LIBRARY_PATH=../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared
./debrunmuddy -h

*)




$toggleDirLoc=FileNameJoin[{Directory[],"/"},OperatingSystem->$OperatingSystem]<>"/";
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
(*,$amaSer,$smolPath*)}];
(**)
$toSoftware=FileNameJoin[{$toGit, "../softwareInstalls/"}];
$reposLoc="/msu/home/m1gsa00/.m2/repository"




Needs["JLink`"]
InstallJava[];

AddToClassPath[FileNameJoin[{$reposLoc,"gov/frb/ma/msu/dynareAntlr/1.0/dynareAntlr-1.0.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"org/antlr/antlr/3.5.2/antlr-3.5.2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"org/antlr/antlr-runtime/3.5.2/antlr-runtime-3.5.2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"SAXON-HE/saxon9-xqj/9-7-0-2/saxon9-xqj-9-7-0-2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"SAXON-HE/saxon9he/9-7-0-2/saxon9he-9-7-0-2.jar"}]];
AddToClassPath[FileNameJoin[{$reposLoc,"xalan-j_2_7_1/xalan/2-7-1/xalan-2-7-1.jar"}]];





(*takes mqxml file representing  rbc model creates an 
AMAmodel and then parses that into a dynare model and creates 
components needed  for c code generation*)
Get["toggleFile.m"]



Get["rbcGenC.m"]



