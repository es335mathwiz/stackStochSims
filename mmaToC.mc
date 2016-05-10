/*Mathematica Creation Date<*Date[]*>*/
/*<*modelCreationInfo*>*/
#include "<*$lagLeadLoc*>"
#include <math.h>
<*stateVectorDefines*>
#define modelShock(n) (shockVec[n])
<*coeffDefines*>
  







void <*functionName*>(double *stateVector,double *parameters,
double * shockVec,
double * aMat,int * jaMat,int *iaMat,double * homotopyAlpha,double * linearizationPoint
)
{
int i;
double bMat[<*modelNumberOfEquations*>];
int ibMat[<*modelNumberOfEquations+1*>];
int jbMat[<*modelNumberOfEquations*>];
if(*homotopyAlpha>=1.0) {
<*opVarDefsSFA*>
<*sparseFunctionAssignmentsA*>
/*for(i=0;i<<*modelNumberOfEquations-numbExog*>;i++){aMat[i]=aMat[i]+shockVec[i];};*/
<*sparseFunctionAssignmentsIA*>
<*sparseFunctionAssignmentsJA*>
} else {
<*opLinVarDefsSFA*>
<*linSparseFunctionAssignmentsA*>
/*for(i=0;i<<*modelNumberOfEquations-numbExog*>;i++){aMat[i]=aMat[i]+shockVec[i];};*/
<*linSparseFunctionAssignmentsIA*>
<*linSparseFunctionAssignmentsJA*>
if(*homotopyAlpha>0.0) {
<*opNLinVarDefsSFA*>
<*nlinSparseFunctionAssignmentsA*>
<*nlinSparseFunctionAssignmentsIA*>
<*nlinSparseFunctionAssignmentsJA*>
for(i=0;i<<*modelNumberOfEquations*>;i++){aMat[i]=aMat[i]+(*homotopyAlpha*bMat[i]);};
}
}
}

