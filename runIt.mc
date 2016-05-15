/*Mathematica Creation Date<*Date[]*>*/
/*<*modelCreationInfo*>*/
#include <stdlib.h>
#include "<*$runItExt*>"



#define PATHLENGTH 1000

int numberOfEquations=<*modelNumberOfEquations*>;
char * namesArray[] =  <*InputForm[ToString /@ allv]*>;
char * paramNamesArray[] = <*InputForm[ToString /@ allcoeffs]*>;;
int numberOfParameters=<*numberOfParameters*>;
int * parameters[]={};
int numDATA=<*dataRows*>;
int numSHOCKS=<*shocksRows*>;
double * theData;

main(int argc, char * argv[])
{
#include "<*$runItInv*>"
#include "run<*outFileString*>LocalDefs.h"
printf("$Id: runIt.mc, 2016 m1gsa00 $\n");

<*functionName*>DataVals=(double *)calloc(numberOfEquations*numDATA,sizeof(double));
for(i=0;i<numDATA;i++){rbcExampleData(i,<*functionName*>DataVals+(i*numberOfEquations));}

<*functionName*>ShockVals=(double *)calloc(numberOfEquations*numSHOCKS,sizeof(double));
for(i=0;i<numSHOCKS;i++){rbcExampleShocks(i,<*functionName*>ShockVals+(i*numberOfEquations));}


processCommandLine(argc,argv,namesArray,numberOfEquations,
paramNamesArray,numberOfParameters,parameters,
	<*functionName*>DataVals,numDATA,numSHOCKS,
	&pathLength,&replications,&t0,&stochasticPathLength,
intControlParameters,doubleControlParameters,flnm);

/*
<*functionName*>PeriodicPointGuesser(parameters,1,<*functionName*>FP);

FPnewt(numberOfEquations,lags,leads,
<*functionName*>,<*functionName*>Derivative,parameters,
<*functionName*>FP,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
maxNumberElements,
failedQ);

*/
}

#include "<*$runItOth*>"

/*
printf("generating perm vec\n");
 generateDraws(1,(stochasticPathLength),(*replications),numSHOCKS,julliardPermVec);
printf("done generating perm vec\n");
*/


/*


altComputeAsymptoticQMatrix(
numberOfEquations,lags,leads,
<*functionName*>,<*functionName*>Derivative,parameters,
<*functionName*>FP,pathLength,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
maxNumberElements,
AMqMatrix,AMqMatrixj,AMqMatrixi,
failedQ
);
*/

/*
if(failedQ[0])
{  printf("problems computing  Q matrix\n");return(1);}
else {printf("computed Q matrix\n");}
*//*
printf("computed Q matrix\n");
for(i=0;i< *pathLength;i++){
<*functionName*>PeriodicPointGuesser(parameters,1,
julliardPathQ+(i *julNEQS));}
*totalTime=dtime(userSystemTime);
printf("after computing Q matrix\ntotalTime=%f,userSystemTime=%f,systemTime=%f\n",
*totalTime,*userSystemTime,*(userSystemTime+1));
printf("using q matrix\n");*/
/*

stochSim(numberOfEquations,lags,leads,pathLength,
<*functionName*>,<*functionName*>Derivative,parameters,
replications,t0,tf,julliardPermVec,
julliardShocks,numberOfShocks,
julliardData,numberOfData,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
maxNumberElements,AMqMatrix,AMqMatrixj,AMqMatrixi,
julliardFP,
julliardPathQ,
failedQ);

*/

