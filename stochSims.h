
#line 102 "stochSims.w"



#define widthIntControlInfo 70
#define widthDoubleControlInfo 50
#define widthIntOutputInfo 20
#define widthDoubleOutputInfo 10
#define useStackQ intControlParameters[0]
#define useLnsrchQ intControlParameters[1]
#define numberOfDebugPairs intControlParameters[2]
#define useIdentityQ intControlParameters[3]
#define maxitsInput intControlParameters[4]
#define maxNumberStackElements intControlParameters[5]
#define maxNumberAimElements intControlParameters[6]
#define numberAlphas intControlParameters[7]
#define numberBetas intControlParameters[8]
#define useFirstDiffQ intControlParameters[9]
#define ma50PivotSearch intControlParameters[10]
#define useQQ intControlParameters[11]
#define terminalConstraintSelection intControlParameters[12]
#define useFixedPoint  0
#define useCrawlingDataPoint  1
#define useWaggingTail  2
#define useFixedDataPoint  3
#define useTailSolutionPlus  4
#define numberVarsToMonitor intControlParameters[13]
#define monitoredVars intControlParameters[14]
/*also reserve next 9 for monotored Vars*/
#define type3Q intControlParameters[24]
#define debugQ intControlParameters[25]
#define numberVarsToShock intControlParameters[26]
#define shockedVars intControlParameters[27]
/*also reserve next 9 for shocked Vars*/
#define tMinusOneQ intControlParameters[37]
#define debugPairs intControlParameters[38]
/*also reserve next 10 for debug pairs*/

#define homotopyXGuess intControlParameters[48]
#define homotopyEasy intControlParameters[49]
#define useBigX 0
#define useBigEasy 1
#define usePreviousHomotopyQ intControlParameters[50]
#define useShockFileQ intControlParameters[51]
#define shockFileOffset intControlParameters[52]
#define dataFileOffset intControlParameters[53]
#define streamingQ intControlParameters[54]
#define ICshockVecLength  intControlParameters[55]
#define ICnumberOfEquation intControlParameters[56]
#define ICnumberOfLags intControlParameters[57]
#define ICnumberOfLeads intControlParameters[58]
#define ICnumberOfParameters intControlParameters[59]
#define ICnumberOfDataValues intControlParameters[60]
#define ICnumberOfShocks intControlParameters[61]
#define ICnumberExog intControlParameters[62]
#define ignoreFailQ intControlParameters[63]


#define tolxInput doubleControlParameters[0]
#define tolfInput doubleControlParameters[1]
#define shrinkFactorInput doubleControlParameters[2]
#define expandFactorInput doubleControlParameters[3]
#define alaminInput doubleControlParameters[4]
#define alfInput doubleControlParameters[5]
#define ma50DropTol doubleControlParameters[6]
#define homotopyAlpha (doubleControlParameters+10)
/*also reserve next 9 for homotopyAlpha*/
#define homotopyBeta (doubleControlParameters+20)
/*also reserve next 9 for homotopyBeta*/
#define ma50Balance doubleControlParameters[30]
#define ma50DropEntry doubleControlParameters[31]
#define ma50DropCol doubleControlParameters[32]
#define shockScalar doubleControlParameters[33]



#define addOneToFailedQ (intOutputInfo[0])++
#define subOneFromFailedQ (intOutputInfo[0])--
#define resetFailedQ (intOutputInfo[0]=0)
#define addOneToNewtonSteps (intOutputInfo[1])++
#define resetNewtonSteps (intOutputInfo[1]=0)
#define addOneToFEvals (intOutputInfo[2])++
#define resetFEvals (intOutputInfo[2]=0)
#define addOneToFDrvEvals (intOutputInfo[3])++
#define resetFDrvEvals (intOutputInfo[3]=0)
#define addOneToShrinkSteps (intOutputInfo[4])++
#define resetShrinkSteps (intOutputInfo[4]=0)
#define addOneToExpandSteps (intOutputInfo[5])++
#define resetExpandSteps (intOutputInfo[5]=0)
#define addOneToLnsrchSteps (intOutputInfo[6])++
#define resetLnsrchSteps (intOutputInfo[6]=0)
#define addOneToHomotopies (intOutputInfo[7])++
#define resetHomotopies (intOutputInfo[7]=0)
#define addOneToHomotopyFailures (intOutputInfo[8])++
#define resetHomotopyFailures (intOutputInfo[8]=0)
#define currentReplication (intOutputInfo[9])
#define currentDate (intOutputInfo[10])

#define assignRealizedTolf doubleOutputInfo[0]
#define resetRealizedTolf (doubleOutputInfo[0]=0)
#define assignRealizedTolx doubleOutputInfo[1]
#define resetRealizedTolx (doubleOutputInfo[1]=0)





/*void cfree();*/
/*void * calloc(unsigned num,int amt);*/
void pathNewt(int * numberOfEquations,int * lags, int * leads,int * pathLength,
void (* vecfunc)(),void (* fdjac)(),double * params,double * shockVec,
double ** fmats, int ** fmatsj, int ** fmatsi,
double ** smats, int ** smatsj, int ** smatsi,
int * maxNumberElements,double * qMat,int * qMatj,int * qMati,
double * fixedPath,double * intercept,double * linearizationPoint,
int * exogRows, int * exogCols, int * exogenizeQ,
double x[],
int *check,double * lastDel,int * intControlParameters,double * doubleControlParameters,
int * intOutputInfo, double * doubleOutputInfo,
int * pathNewtMa50bdJob,
int * pathNewtMa50bdIq,
double * pathNewtMa50bdFact,
int * pathNewtMa50bdIrnf,
int * pathNewtMa50bdIptrl,
int * pathNewtMa50bdIptru
);
long ignuin(long low,long high);
void phrtsd(char* phrase,long* seed1,long* seed2);
void setall(long iseed1,long iseed2);

void generateDraws(int t0Index,int tfIndex,int replications,int shocksAvailable,
int * iarray,char * str);

