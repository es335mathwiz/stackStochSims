#include <stdio.h>
#include "stochSims.h"
/*set maximal dimension constants*/
#define PATHLENGTH 1000
#define REPLICATIONS 1000
#define PRINTMAX 12
#define FALSE 0
#define TRUE 1
void processCommandLine(int argc, char * argv[],char ** namesArray,int modelNEQS,char ** paramNamesArray,int numberOfParameters,double * parameters,
double * dataValues,int numberDataValues,int numberShockValues,
int * pathLength,int * replications,int * t0,int * stochasticPathLength,
int* intControlParameters,double * doubleControlParameters,char * flnm);
void fPrintMathDbl(FILE * file,int length,double * matrix,char *  matrixName);
void fPrintMathInt(FILE * file,int length,int * matrix,char *  matrixName);




