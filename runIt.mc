/*Mathematica Creation Date<*Date[]*>*/
/*<*modelCreationInfo*>*/
#include <stdlib.h>
#include "<*$runItExt*>"

main(int argc, char * argv[])
{
#include "<*$runItInv*>"
#include "run<*outFileString*>LocalDefs.h"
printf("$Id: runIt.mc,v 1.5 2001/06/19 19:43:17 m1gsa00 Exp m1gsa00 $\n");
/*obtain dimensions for model*/
<*functionName*>ModelDimensions(&numberOfEquations,&lags,&leads,
	&numberOfParameters,&numberOfDataValues,&numberOfShocks,&numberExog);

/*allocate space for objects that do not depend on command line switches*/

allocLinearTerminator(numberOfEquations,lags,leads,
numberExog,
maxNumberElements,
&upsilonMatrix,&upsilonMatrixj,&upsilonMatrixi,
&hMat,&hMatj,&hMati,
&hzMat,&hzMatj,&hzMati,
&cstar,&cstarj,&cstari,
&AMqMatrix,&AMqMatrixj,&AMqMatrixi,
& rootr,&rooti,
&AMbMatrix,&AMbMatrixj,&AMbMatrixi,
&phiInvMat,&phiInvMatj,&phiInvMati,
&fmat,&fmatj,&fmati,
&varthetaZstar,&varthetaZstarj,&varthetaZstari,
&impact,&impactj,&impacti,
&varthetaC,&varthetaCj,&varthetaCi,
&selectZmat,&selectZmatj,&selectZmati
);
/*space for data and shocks*/
allocShocksData(numberOfEquations,numberOfShocks,numberOfDataValues,
	&<*functionName*>ShockVals,&<*functionName*>DataVals,
	&<*functionName*>ZeroShock);
/*space for qmatrix*/
/*allocAltComputeAsymptoticQ(numberOfEquations,lags,leads,spaMaxNumberElements,
	&AMqMatrix,&AMqMatrixj,&AMqMatrixi,&rootr,&rooti);*/
/*space for bmatrix*/
/*allocAltComputeAsymptoticQ(numberOfEquations,lags,leads,spaMaxNumberElements,
	&AMbMatrix,&AMbMatrixj,&AMbMatrixi,&brootr,&brooti);*/
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after storage allocations\n totalTime=%f,\
	userSystemTime=%f,systemTime=%f\n",
	*totalTime,*userSystemTime,*(userSystemTime+1));



/*initialize data and shocks*/
for(i=0;i<numberOfDataValues;i++){<*functionName*>Data(i,
	<*functionName*>DataVals+(i*numberOfEquations));}
for(i=0;i<numberOfShocks;i++){<*functionName*>Shocks(i,
	<*functionName*>ShockVals+(i*numberOfEquations));}


processCommandLine(argc,argv,namesArray,numberOfEquations,
paramNamesArray,numberOfParameters,parameters,
	<*functionName*>DataVals,numberOfDataValues,
	&pathLength,&replications,&t0,&stochasticPathLength,
intControlParameters,doubleControlParameters,flnm);


/*open output file*/
    outFile=fopen(flnm,"w");



/*allocate space for objects that depend on command line switches*/
allocMa50(numberOfEquations,lags,leads,pathLength,pathLength*MAXELEMENTS+SPAMAXELEMENTS,
		  &ma50bdIptru,
		  &ma50bdIptrl,
		  &ma50bdIrnf,
		  &ma50bdFact,
		  &ma50bdIq,
		  &ma50bdJob);
allocMa50(numberOfEquations,lags,leads,1,MAXELEMENTS+SPAMAXELEMENTS,
		  &cmpma50bdIptru,
		  &cmpma50bdIptrl,
		  &cmpma50bdIrnf,
		  &cmpma50bdFact,
		  &cmpma50bdIq,
		  &cmpma50bdJob);
/*space for FP and for newton step workspace*/
allocFPNewt(numberOfEquations,lags,leads,
	pathLength,MAXELEMENTS,
&<*functionName*>FP,
&<*functionName*>Intercept,
&fmats,&fmatsj,&fmatsi,&smats,&smatsj,&smatsi);
/*space for path*/
allocPathNewt(numberOfEquations,lags,leads,
	pathLength,replications,stochasticPathLength,
&<*functionName*>PathQ,&<*functionName*>ZeroPathQ,
&<*functionName*>EasyPathQ,&<*functionName*>TargetPathQ);
/*space for stochSims sucess record*/
allocStochSims(stochasticPathLength,replications,&failedQ);
/*initialize  whole path to data values at t0*/
for(i=0;i<lags+pathLength+leads+stochasticPathLength;i++){
  for(j=0;j<numberOfEquations;j++){
	<*functionName*>ZeroPathQ[i* numberOfEquations+j]=
	  <*functionName*>DataVals[(i+t0)*numberOfEquations+j];
	<*functionName*>PathQ[i* numberOfEquations+j]=
	  <*functionName*>DataVals[(i+t0)*numberOfEquations+j];
  }}



/*initialize  whole path to data values at t0*/
for(i=0;i<lags+1+leads;i++){
  for(j=0;j<numberOfEquations;j++){
	<*functionName*>FP[i* numberOfEquations+j]=
	  <*functionName*>DataVals[(i+pathLength-1+t0)*numberOfEquations+j];
  }}









<*functionName*>PeriodicPointGuesser(parameters,1,<*functionName*>FP);

printf("initiating FP solution computation\n");
FPnewt(&numberOfEquations,&lags,&leads,
<*functionName*>,<*functionName*>Derivative,parameters,
<*functionName*>FP,
<*functionName*>FP,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo);
printf("computed FP solution\n");
/*initialize  whole path execept lags to fp*/
for(i=lags;i<lags+1+leads;i++){
  for(j=0;j<numberOfEquations;j++){
	<*functionName*>TargetPathQ[i* numberOfEquations+j]=<*functionName*>FP[j];
	<*functionName*>PathQ[i* numberOfEquations+j]=<*functionName*>FP[j];
  }}

/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after fixed point computation\n totalTime=%f,userSystemTime=%f,systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));






printf("generating perm vec\n");
allocGenerateDraws(1,stochasticPathLength,replications,
&<*functionName*>PermVec);
 generateDraws(1,(stochasticPathLength),replications,numberOfShocks,<*functionName*>PermVec,"justForNow");
printf("done generating perm vec\n");

/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after generating draws\n totalTime=%f,userSystemTime=%f,\
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));








/*compute asymptotic Q constraint*/
if(!useIdentityQ){
maxNumberAimElements=SPAMAXELEMENTS;
altComputeAsymptoticQMatrix(
&numberOfEquations,&lags,&leads,
<*functionName*>,<*functionName*>Derivative,parameters,
<*functionName*>ZeroShock,
<*functionName*>FP,pathLength,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&spaMaxNumberElements,
AMqMatrix,AMqMatrixj,AMqMatrixi,&auxInit,&qRows,rootr,rooti,
failedQ,0,
 intControlParameters, doubleControlParameters,
 intOutputInfo,  doubleOutputInfo
);
/*
<*functionName*>Upsilon(parameters,
upsilonMatrix,upsilonMatrixj,upsilonMatrixi
			  );
linearTerminator(&numberOfEquations,&lags,&leads,
&numberExog,<*functionName*>FP,
<*functionName*>,<*functionName*>Derivative,<*functionName*>ExogH,
parameters,
upsilonMatrix,upsilonMatrixj,upsilonMatrixi,
&spaMaxNumberElements,
hMat,hMatj,hMati,
hzMat,hzMatj,hzMati,
AMqMatrix,AMqMatrixj,AMqMatrixi,
&auxInit,&qRows,rootr,rooti,
AMbMatrix,AMbMatrixj,AMbMatrixi,
phiInvMat,phiInvMatj,phiInvMati,
fmat,fmatj,fmati,
varthetaZstar,varthetaZstarj,varthetaZstari,
impact,impactj,impacti,
varthetaC,varthetaCj,varthetaCi,
failedQ
);


*/

fprintf(outFile,"<*functionName*>AuxInitQRows={%d,%d}\n",auxInit,qRows);
fPrintMathDbl(outFile,(numberOfEquations*(leads+lags)),
      rootr,"<*functionName*>Rootr");
fPrintMathDbl(outFile,(numberOfEquations*(leads+lags)),
      rooti,"<*functionName*>Rooti");

/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after computing Q,max elems=%d\n totalTime=%f,userSystemTime=%f,\
systemTime=%f\n",spaMaxNumberElements,*totalTime,*userSystemTime,*(userSystemTime+1));
}
printf("qmatrix has %d rows of which %d are auxiliary inits",qRows,auxInit);

if(qRows <numberOfEquations*leads){
printf("qmatrix has %d rows, need %d so using identity matrix terminal condition\n",qRows,numberOfEquations*leads);
altComputeAsymptoticIMatrix(
&numberOfEquations,&lags,&leads,
AMqMatrix,AMqMatrixj,AMqMatrixi,
failedQ
);
}

/*
maxNumberAimElements=SPAMAXELEMENTS;
spaMaxNumberElements=SPAMAXELEMENTS;
obtainSparseReducedForm(&spaMaxNumberElements,numberOfEquations* leads,
numberOfEquations* (leads+lags),
AMqMatrix,AMqMatrixj,AMqMatrixi,
AMbMatrix,AMbMatrixj,AMbMatrixi);*/
/*time used so far?*/
/* *totalTime=dtime_(userSystemTime);
printf("after computing B\n totalTime=%f,userSystemTime=%f,
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));
*/








/*use reduced form to compute rest of path*/
/*for(i=0;i< pathLength+leads+stochasticPathLength;i++){
	applySparseReducedForm(numberOfEquations,
		numberOfEquations* lags,
		<*functionName*>TargetPathQ+(i *numberOfEquations),<*functionName*>FP,
		AMbMatrix,AMbMatrixj,AMbMatrixi,
<*functionName*>TargetPathQ+((i * numberOfEquations)+(numberOfEquations*lags)));
for(j=0;j<numberOfEquations;j++){
<*functionName*>TargetPathQ[((i * numberOfEquations)+(numberOfEquations*lags))+j]=
<*functionName*>FP[j+(numberOfEquations*lags)%
	(numberOfEquations*(lags+leads+1))];}
}*/

/*set terminal time and call stochSim*/
tf=t0+stochasticPathLength-1;
printf("saving values for variable in file named %s\n",flnm);
fprintf(outFile,"<*functionName*>RunParams={%d,%d,%d,%d,%d,%d,%d};\n",
    numberOfEquations,lags,leads,
     pathLength,t0,stochasticPathLength,replications);
fPrintMathInt(outFile,replications * (stochasticPathLength),
      <*functionName*>PermVec,"<*functionName*>PermVec");
fPrintMathDbl(outFile,(numberOfEquations*numberOfDataValues),<*functionName*>DataVals,"<*functionName*>Data");
fPrintMathDbl(outFile,(numberOfEquations*numberOfShocks),<*functionName*>ShockVals,"<*functionName*>Shocks");

/*
<*functionName*>(<*functionName*>FP,parameters,<*functionName*>ZeroShock,cstar,cstarj,cstari);


computeIntercept(&spaMaxNumberElements,numberOfEquations,lags,leads,
numberExog,
hzMat,hzMatj,hzMati,
selectZmat,selectZmatj,selectZmati,
varthetaZstar,varthetaZstarj,varthetaZstari,
impact,impactj,impacti,
varthetaC,varthetaCj,varthetaCi,
cstar,cstarj,cstari,
<*functionName*>FP,
<*functionName*>TargetPathQ,
<*functionName*>Intercept);
*/

maxNumberStackElements=MAXELEMENTS;
*ma50bdJob=1;
pathNewt(&numberOfEquations,&lags,&leads,&pathLength,
<*functionName*>,<*functionName*>Derivative,parameters,
<*functionName*>ZeroShock,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,AMqMatrix,AMqMatrixj,AMqMatrixi,
<*functionName*>FP,<*functionName*>Intercept,<*functionName*>FP,
<*functionName*>ZeroPathQ,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo,
ma50bdJob,
ma50bdIq,
ma50bdFact,
ma50bdIrnf,
ma50bdIptrl,
ma50bdIptru);
fPrintMathInt(outFile,widthIntOutputInfo,intOutputInfo,
"<*functionName*>ZeroShockIntOutputInfo");
fPrintMathDbl(outFile,widthIntOutputInfo,doubleOutputInfo,
"<*functionName*>ZeroShockDoubleOutputInfo");
printf("max elems for first pathNewt=%d\n",maxNumberElements);
maxNumberElements=MAXELEMENTS;
maxNumberStackElements=MAXELEMENTS;
fPrintMathDbl(outFile,(numberOfEquations*(leads+pathLength+lags)),
      <*functionName*>ZeroPathQ,"<*functionName*>ZeroShockResults");
fPrintMathInt(outFile,1,
failedQ,"<*functionName*>ZeroShocksFailedQ");
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after using Q matrix\ntotalTime=%f,userSystemTime=%f,\
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));
*cmpma50bdJob=1;
for(i=0;i<widthIntOutputInfo*replications;i++){
intOutputInfo[i]=0;
doubleOutputInfo[i]=0;
}
stochSim(&numberOfEquations,&lags,&leads,&pathLength,
<*functionName*>,<*functionName*>Derivative,parameters,
&numberExog,upsilonMatrix,upsilonMatrixj,upsilonMatrixi,<*functionName*>ExogH,
&replications,&t0,&tf,<*functionName*>PermVec,
<*functionName*>ShockVals,&numberOfShocks,
<*functionName*>DataVals,&numberOfDataValues,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,AMqMatrix,AMqMatrixj,AMqMatrixi,
<*functionName*>ZeroPathQ,<*functionName*>Intercept,<*functionName*>FP,
<*functionName*>PathQ,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo,
ma50bdJob,
ma50bdIq,
ma50bdFact,
ma50bdIrnf,
ma50bdIptrl,
ma50bdIptru,
cmpma50bdJob,
cmpma50bdIq,
cmpma50bdFact,
cmpma50bdIrnf,
cmpma50bdIptrl,
cmpma50bdIptru
);
printf("max elems for first stochSim=%d\n",maxNumberElements);

fPrintMathDbl(outFile,(replications * numberOfEquations*(stochasticPathLength+lags)),
      <*functionName*>PathQ,"<*functionName*>Results");
fPrintMathInt(outFile,replications*widthIntOutputInfo,intOutputInfo,
"<*functionName*>IntOutputInfo");
fPrintMathDbl(outFile,replications*widthIntOutputInfo,doubleOutputInfo,
"<*functionName*>DoubleOutputInfo");
fPrintMathInt(outFile,replications*stochasticPathLength,
failedQ,"<*functionName*>failedQ");
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after using Q matrix\ntotalTime=%f,userSystemTime=%f,\
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));


     fclose(outFile);




cfreeLinearTerminator(
&upsilonMatrix,&upsilonMatrixj,&upsilonMatrixi,
&hMat,&hMatj,&hMati,
&hzMat,&hzMatj,&hzMati,
&cstar,&cstarj,&cstari,
&AMqMatrix,&AMqMatrixj,&AMqMatrixi,
& rootr,&rooti,
&AMbMatrix,&AMbMatrixj,&AMbMatrixi,
&phiInvMat,&phiInvMatj,&phiInvMati,
&fmat,&fmatj,&fmati,
&varthetaZstar,&varthetaZstarj,&varthetaZstari,
&impact,&impactj,&impacti,
&varthetaC,&varthetaCj,&varthetaCi,
&selectZmat,&selectZmatj,&selectZmati
);

cfreeGenerateDraws(&<*functionName*>PermVec);
cfreeShocksData(&<*functionName*>ShockVals,&<*functionName*>DataVals,
	&<*functionName*>ZeroShock);
/*cfreeAltComputeAsymptoticQ(
&AMqMatrix,&AMqMatrixj,&AMqMatrixi,&rootr,&rooti);
cfreeAltComputeAsymptoticQ(
&AMbMatrix,&AMbMatrixj,&AMbMatrixi,&brootr,&brooti);*/
cfreePathNewt(&<*functionName*>PathQ);
cfreePathNewt(&<*functionName*>ZeroPathQ);
cfreeFPNewt(lags,pathLength,
&<*functionName*>FP,
&<*functionName*>Intercept,
&fmats,&fmatsj,&fmatsi,&smats,&smatsj,&smatsi);
cfreeStochSims(&failedQ);
cfreeMa50(&ma50bdIptru,
		  &ma50bdIptrl,
		  &ma50bdIrnf,
		  &ma50bdFact,
		  &ma50bdIq,
		  &ma50bdJob);
cfreeMa50(&cmpma50bdIptru,
		  &cmpma50bdIptrl,
		  &cmpma50bdIrnf,
		  &cmpma50bdFact,
		  &cmpma50bdIq,
		  &cmpma50bdJob);


return(0);

}

#define SHOCKS <*shocksCols*>
void modData(int numberOfEquations,int numberDataValues,double * dataVals,
			 int vbl,int t0,int tf,double val1,double val2)
{
  int t;
  for(t=t0;t<=tf&&t<numberDataValues;t++){
dataVals[t*numberOfEquations+vbl]=dataVals[t*numberOfEquations+vbl]+
  (t-t0)*val2/(tf-t0) + (tf-t)*val1/(tf-t0);
  }
}
void modDataAbs(int numberOfEquations,int numberDataValues,double * dataVals,
			 int vbl,int t0,int tf,double val1,double val2)
{
  int t;
  for(t=t0;t<=tf&&t<numberDataValues;t++){
dataVals[t*numberOfEquations+vbl]=(t-t0)*val2/(tf-t0) + (tf-t)*val1/(tf-t0);
  }
}
 
 
#include "<*$runItOth*>"

