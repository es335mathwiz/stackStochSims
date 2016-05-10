/*Mathematica Creation Date<*Date[]*>*/
/*<*modelCreationInfo*>*/
#include "<*$lagLeadLoc*>"
#include <math.h>
static double maxarg1,maxarg2;
#include <math.h>

double FMAX(double a,double b)
{
  return(a > b ? a : b);
}
double FMIN(double a,double b)
{
  return(a < b ? a : b);
}
double FABS(double a)
{
  return(a > 0 ? a: -a);
}

double doRightSmaller(double a,double b)
{
  return(a < b ? 0 : 1);
}
double doSign(double a)
{
  /*  return(fabs(a) >0.01?(a > 0 ? 1 : -1):2*a) ;*/
  return(a > 0 ? 1 : -1) ;

}
#define modelShock(n) (0)  


<*stateVectorDefines*>
<*coeffDefines*>  




void <*functionName*>PeriodicPointGuesser
(double * parameters,int period,
	double guessVector[<*modelColumns*>][<*modelNumberOfEquations*>])
{
int i,j;
double svalue;
int timeOffset;
for(timeOffset=0;
	timeOffset<period+ <*modelColumns*> - 1;
			timeOffset++)
	{
<*periodPointGuesserAssignments*>
}
}

void <*functionName*>ModelDimensions(int * numberOfEquations, int * lags,
int * leads, int * numberOfParameters,
int * numberOfDataValues, int * numberOfShocks,int * numberExogenous)
{
*numberOfEquations=<*modelNumberOfEquations*>;
*lags=<*-ll[[1]]*>;
*leads=<*ll[[-1]]*>;
*numberOfParameters=<*numberOfParameters*>;
*numberOfDataValues=<*dataRows*>;
*numberOfShocks=<*shocksRows*>;
*numberExogenous=<*numbExog*>;
}
void <*functionName*>Upsilon(double *parameters,
double * aMat,int * jaMat,int *iaMat
)
{
<*upsilonMatA*>
<*upsilonMatIA*>
<*upsilonMatJA*>
}
void <*functionName*>ExogH(double *parameters,double *stateVector,
double * aMat,int * jaMat,int *iaMat
)
{
<*exogHMatA*>
<*exogHMatIA*>
<*exogHMatJA*>
}
void <*functionName*>SelectZ(double * aMat,int * jaMat,int *iaMat
)
{
<*selectZMatA*>
<*selectZMatIA*>
<*selectZMatJA*>
}
