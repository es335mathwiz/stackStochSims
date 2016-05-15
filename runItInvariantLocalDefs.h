
/*declare prototypes for functions*/
float  dtime_(float * userSystemTime);
double atof();


FILE * outFile;
  char outFileName[250];
static char flnm[50] = "stochOut.m";
/*a counter*/
int i;int j;
/*modelDimensions call determines these*/
int  numberOfEquations;
int  lags;
int  leads;
int  numberOfParameters;
int  numberOfDataValues;
int  numberOfShocks;
int  numberExog;


int intControlParameters[widthIntControlInfo];
double doubleControlParameters[widthDoubleControlInfo];
 int intOutputInfo[REPLICATIONS*widthIntOutputInfo];
double doubleOutputInfo[REPLICATIONS*widthDoubleOutputInfo];


int qRows=0;
 int auxInit=0;
 int aZero=0;

/*processCommandLine() determines defaults for these*/
int  replications;
int  pathLength;
int  stochasticPathLength;
int  t0;
int  tf;

/*timing routine variables*/
float  totalTime[1];
float  userSystemTime[2];
/*workspace*/
double **fmats;int  **fmatsj;int  **fmatsi;
double **smats;int  **smatsj;int  **smatsi;
/*success indicators for stochSims*/
int failedQ[1]={0};
/*csr q matrix*/
double * AMqMatrix;
int * AMqMatrixj;
int * AMqMatrixi;
/*csr b matrix*/
double * AMbMatrix;
int * AMbMatrixj;
int * AMbMatrixi;
 double * rootr;
 double * rooti;
 double * brootr;
 double * brooti;
double*upsilonMatrix;int*upsilonMatrixj;int*upsilonMatrixi;
double*hMat;int*hMatj;int*hMati;
double*hzMat;int*hzMatj;int*hzMati;
double*cstar;int*cstarj;int*cstari;
double*phiInvMat;int*phiInvMatj;int*phiInvMati;
double*fmat;int*fmatj;int*fmati;
double*impact;int*impactj;int*impacti;
double*selectZmat;int*selectZmatj;int*selectZmati;
double*varthetaC;int*varthetaCj;int*varthetaCi;
double*varthetaZstar;int*varthetaZstarj;int*varthetaZstari;

 int * ma50bdJob;
 int * ma50bdIq;
 double * ma50bdFact;
 int * ma50bdIrnf;
 int * ma50bdIptrl;
 int * ma50bdIptru;
 int * cmpma50bdJob;
 int * cmpma50bdIq;
 double * cmpma50bdFact;
 int * cmpma50bdIrnf;
 int * cmpma50bdIptrl;
 int * cmpma50bdIptru;
 int sysDim;



