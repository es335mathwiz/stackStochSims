
#line 243 "stochSims.w"

/*what goes here?*/
#include <stdio.h>
#ifndef STOCHSIMS_H
#define STOCHSIMS_H
#endif

#include <stdio.h>
#include <math.h>
#include "mpi.h"
#define TRUE 1
#define DATA_MSG_TAG 0
#define RESULT_MSG_TAG 1
#define HALT_MSG_TAG 2

#define BUFFER_SIZE 1024

#define RESULT_ELEMENT_COUNT 3

void sendDataMessage(int,int*);
void sendHaltMessage(int);
void buildResultType(double*, int*, int*, int, int, MPI_Datatype*);
void error(int, int, int, int);
void stochSim(
int * numberOfEquations,int * lags, int * leads,int * pathLength,
void (* vecfunc)(),void (* fdjac)(),int * exogRow, int * exogCol, int * exogenizeQ,
double easyX[],double targetX[],int * exogQ,
double * params,
int * numberExog,
double * upsilonmat,int * upsilonmatj,int * upsilonmati,void (* exdfunc)(),
int * replications,
int * t0,int * tf,int * permVecs,
double * shockTable,int * shocksAvailable,
double * dataTable,int * dataAvailable,
double ** fmats, int ** fmatsj, int ** fmatsi,
double ** smats, int ** smatsj, int ** smatsi,
int * maxNumberElements,double * qMat,int * qMatj,int * qMati,
double * fixedPoint,double * intercept,
double x[],
int *failedQ,int * intControlParameters,double * doubleControlParameters,
int * intOutputInfo, double * doubleOutputInfo,
int * pathNewtMa50bdJob,
int * pathNewtMa50bdIq,
double * pathNewtMa50bdFact,
int * pathNewtMa50bdIrnf,
int * pathNewtMa50bdIptrl,
int * pathNewtMa50bdIptru,
int * compXMa50bdJob,
int * compXMa50bdIq,
double * compXMa50bdFact,
int * compXMa50bdIrnf,
int * compXMa50bdIptrl,
int * compXMa50bdIptru
);
