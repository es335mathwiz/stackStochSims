
/*Mathematica Creation Date<*Date[]*>*/
/*<*modelCreationInfo*>*/
#include "<*$lagLeadLoc*>"
#include <math.h>
<*stateVectorDefines*>
#define modelShock(n) (shockVec[n])
<*coeffDefines*>
  







void <*functionName*>Derivative(double *stateVector,double *parameters,
double * shockVec,
double * aMat,int * jaMat,int *iaMat,double * homotopyAlpha,double * linearizationPoint
)
{int i;
double bMat[<*bLength*>];
int ibMat[<*modelNumberOfEquations+1*>];
int jbMat[<*bLength*>];
double cMat[<*bLength*>];
int icMat[<*modelNumberOfEquations+1*>];
int jcMat[<*bLength*>];
int aOne=1;int ierr;int maxNumberHElements;
int hrows=<*modelNumberOfEquations*>;
int hcols=<*modelColumns*modelNumberOfEquations*>;
double okay[<*spaceForTempVars*>];
if(*homotopyAlpha>=1.0) {
<*opVarDefsDrvSFA*>


<*sparseFunctionDerivativeAssignmentsA*>
<*sparseFunctionDerivativeAssignmentsIA*>
<*sparseFunctionDerivativeAssignmentsJA*>
} else {
<*opLinVarDefsDrvSFA*>
<*linSparseFunctionDerivativeAssignmentsA*>
<*linSparseFunctionDerivativeAssignmentsIA*>
<*linSparseFunctionDerivativeAssignmentsJA*>
/*initialize cMat to zero sparse matrix*/
for(i=0;i<=<*modelNumberOfEquations+1*>;i++){icMat[i]=1;}
for(i=0;i<<*bLength*>;i++){cMat[i]=0;};
if(*homotopyAlpha>0.0) {
<*opNLinVarDefsDrvSFA*>
<*nlinSparseFunctionDerivativeAssignmentsA*>
<*nlinSparseFunctionDerivativeAssignmentsIA*>
<*nlinSparseFunctionDerivativeAssignmentsJA*>
for(i=0;i<<*bLength*>;i++){cMat[i]=cMat[i]*(*homotopyAlpha);};
}
maxNumberHElements=<*bLength*>;
aplb_(&hrows,&hcols,&aOne,bMat,jbMat,ibMat,cMat,jcMat,icMat,
aMat,jaMat,iaMat,&maxNumberHElements,okay,&ierr);
}
}
