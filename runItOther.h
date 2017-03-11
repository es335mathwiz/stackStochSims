#ifdef __APPLE__
#include<strings.h>
#endif
#ifdef __linux__
#include<string.h>
#endif
/* */

void modData(int numberOfEquations,int numberDataValues,double * dataVals,
			 int vbl,int t0,int tf,double val1,double val2)
{
  int t;
  for(t=t0;t<=tf&&t<numberDataValues;t++){
	if(t0==tf) {
dataVals[t*numberOfEquations+vbl]=dataVals[t*numberOfEquations+vbl]+
  (val2+val1)/2;} else {
dataVals[t*numberOfEquations+vbl]=dataVals[t*numberOfEquations+vbl]+
  (t-t0)*val2/(tf-t0) + (tf-t)*val1/(tf-t0);}
  }
}
void modDataAbs(int numberOfEquations,int numberDataValues,double * dataVals,
			 int vbl,int t0,int tf,double val1,double val2)
{
  int t;
  for(t=t0;t<=tf&&t<numberDataValues;t++){
	if(t0==tf) {
dataVals[t*numberOfEquations+vbl]= (val2+val1)/2;} else {
	dataVals[t*numberOfEquations+vbl]=(t-t0)*val2/(tf-t0) + (tf-t)*val1/(tf-t0);}
  }
}/* */



void processCommandLine(int argc, char * argv[],char ** namesArray,int modelNEQS,char ** paramNamesArray,int numberOfParameters,double * parameters,
double * dataValues,int numberDataValues,int numShockValues,
int * pathLength,int * replications,int * t0,int * stochasticPathLength,
int * intControlParameters,double* doubleControlParameters,char * flnm)
{
  float aFloat;int i;int anInt;
 int pl;int t1;int t2; double val1; double val2; int vbl;
/*setup defaults*/
useLnsrchQ=FALSE;
useStackQ=FALSE;
useIdentityQ=FALSE;
useFirstDiffQ=FALSE;
 alaminInput=1e-10;
 alfInput=1e-4;
 expandFactorInput=1.3;
 shrinkFactorInput=0.7;
 tolfInput=1e-10;
  tolxInput=1e-10;
 maxitsInput=100;
*replications=1;
*stochasticPathLength=1;
*pathLength=1;
*t0=0;
numberAlphas=1;
homotopyAlpha[0]=1.0;
numberBetas=1;
homotopyBeta[0]=1.0;
while(argc>1&&argv[1][0] == '-')
{
printf("processing command line args\n");
 switch(argv[1][1]){
   case 'p':
i=0;
while((strcmp(argv[2],paramNamesArray[i])) && (i <modelNEQS))i++;
if(i==modelNEQS){
printf("i don't know the parameter %s: ignoring this parmeter value pair\n",argv[2]);} else {vbl = i;}
         sscanf(argv[3],"%f",&aFloat);val1=(double)aFloat;
         printf("got %d for param %s  and %f for value\n",vbl,paramNamesArray[i],
	val1);
		 parameters[i]=val1;
         argc--;argv++;
         argc--;argv++;
         break;
   case 'v':
i=0;
while((strcmp(argv[2],namesArray[i])) && (i <modelNEQS))i++;
if(i==modelNEQS){
printf("i don't know the variable %s: ignoring this variable value pair\n",argv[2]);} else {vbl = i;}
         t1=(int)atoi(argv[3]);
         t2=(int)atoi(argv[4]);
         sscanf(argv[5],"%f",&aFloat);val1=(double)aFloat;
         sscanf(argv[6],"%f",&aFloat);val2=(double)aFloat;
         printf("got %d for vbl %s and (%d,%d) (%f,%f)\n",vbl,namesArray[i],
	t1,t2,val1,val2);
		 modData(modelNEQS,numberDataValues,dataValues,vbl,t1,t2,val1,val2);
         argc--;argv++;
         argc--;argv++;
         argc--;argv++;
         argc--;argv++;
         argc--;argv++;
         break;
   case 'V':
i=0;
while((strcmp(argv[2],namesArray[i])) && (i <modelNEQS))i++;
if(i==modelNEQS){
printf("i don't know the variable %s: ignoring this variable value pair\n",argv[2]);} else {vbl = i;}
         t1=(int)atoi(argv[3]);
         t2=(int)atoi(argv[4]);
         sscanf(argv[5],"%f",&aFloat);val1=(double)aFloat;
         sscanf(argv[6],"%f",&aFloat);val2=(double)aFloat;
         printf("got %d for vbl %s and (%d,%d) (%f,%f)\n",vbl,namesArray[i],
	t1,t2,val1,val2);
		 modDataAbs(modelNEQS,numberDataValues,dataValues,vbl,t1,t2,val1,val2);
         argc--;argv++;
         argc--;argv++;
         argc--;argv++;
         argc--;argv++;
         argc--;argv++;
         break;
   case 'l':
         pl=atoi(argv[2]);
     printf("got %d for path length\n",pl);
     if(pl>PATHLENGTH)
         {
       *pathLength=PATHLENGTH;
       printf("setting pathlength to maximum=%d\n",PATHLENGTH);
       } else   if(pl<1){
       *pathLength=1;
       printf("setting pathlength to 1\n");
       } else 
     {*pathLength=pl;}
         argc--;argv++;
         break;
   case 'r':
         pl=atoi(argv[2]);
     printf("got %d for replications\n",pl);
     if(pl>REPLICATIONS||pl<1)
         {
       *replications=REPLICATIONS;
       printf("setting repetitions to maximum=%d\n",REPLICATIONS);
       } else { *replications = pl;}
         argc--;argv++;
         break;
   case 'a':
         pl=atoi(argv[2]);
     printf("got %d for t0\n",pl);
     if(pl>numShockValues||pl<0)
         {
       *t0=numShockValues;
       printf("initial t0 to maximum=%d\n",numShockValues);
       } else { *t0 = pl;}
         argc--;argv++;
         break;
   case 's':
         *stochasticPathLength=atoi(argv[2]);
     printf("got %d for stochasticPathLength\n",*stochasticPathLength);
     if(*stochasticPathLength<1)
         {
       *stochasticPathLength=1;
       printf("setting tf to 1\n");
       }
         argc--;argv++;
         break;
   case 'N':
         (maxitsInput)=atoi(argv[2]);
     printf("got %d for maxits\n",(maxitsInput));
     if((maxitsInput)<1)
         {
       (maxitsInput)=1;
       printf("setting maxits to 1\n");
       }
         argc--;argv++;
         break;
   case 'F':
         sscanf(argv[2],"%f",&aFloat);
		 tolfInput=(double)aFloat;
     printf("got %e for tolfInput\n",tolfInput);
         argc--;argv++;
         break;
   case 'X':
         sscanf(argv[2],"%f",&aFloat);
		 tolxInput=(double)aFloat;
     printf("got %e for tolxInput\n",tolxInput);
         argc--;argv++;
         break;
   case 'H':
         sscanf(argv[2],"%d",&anInt);
		 numberAlphas=anInt;
		 if(anInt>10){numberAlphas=10; printf("only using first 10\n");}
         argc--;argv++;
		 for(i=0;i<anInt;i++){
         sscanf(argv[2],"%f",&aFloat);
		 if(i<10){homotopyAlpha[i]=(double)aFloat;}
     printf("got %e for homotopyAlpha[%d]\n",aFloat,i);
	 argc--;argv++;}
         break;
   case 'D':
         sscanf(argv[2],"%d",&anInt);
		 numberBetas=anInt;
		 if(anInt>10){numberBetas=10; printf("only using first 10\n");}
         argc--;argv++;
		 for(i=0;i<anInt;i++){
         sscanf(argv[2],"%f",&aFloat);
		 if(i<10){homotopyBeta[i]=(double)aFloat;}
     printf("got %e for homotopyBeta[%d]\n",aFloat,i);
	 argc--;argv++;}
         break;
   case 'K':
         sscanf(argv[2],"%f",&aFloat);
		 shrinkFactorInput=(double)aFloat;
     printf("got %e for shrinkFactorInput\n",shrinkFactorInput);
         argc--;argv++;
         break;
   case 'E':
         sscanf(argv[2],"%f",&aFloat);
		 expandFactorInput=(double)aFloat;
     printf("got %e for expandFactorInput\n",expandFactorInput);
         argc--;argv++;
         break;
   case 'L':
         useLnsrchQ=TRUE;
     printf("got flag for using lnsrch algorithm\n");
         break;
   case 'S':
         useStackQ=TRUE;
     printf("got flag for using stack algorithm\n");
         break;
   case 'I':
         useIdentityQ=TRUE;
         useFirstDiffQ=FALSE;
     printf("got flag for using identity matrix\n");
         break;
   case 'J':
         useFirstDiffQ=TRUE;
         useIdentityQ=FALSE;
     printf("got flag for using first difference matrix\n");
         break;
   case 'h':
     printf("\n-l <stack pathlength>\n"); 
     printf("-s <stochastic pathlength>\n");
     printf("-r <number of replications>\n");
     printf("-f <output filename>\n");
     printf("-L  use lnsearch\n");
     printf("-I  use identity matrix terminal condition\n");
     printf("-a <offset into datamatrix>\n");
     printf("-X <x convergence tolerance>\n");
     printf("-F <f(x) convergence tolerance>\n");
     printf("-N <maximum number of newton steps>\n");
     printf("-E <expansion factor>\n");
     printf("-K <shrinkage factor>\n");
     printf("-v <variableName> <dataPt 0 > <dataPtf> <incrementVal0> <incrementValf>\n");
     printf("-V <variableName> <dataPt 0 > <dataPtf> <val0> <valf>\n");
     printf("-p <parameterName> <valf>\n");
        break;
   case 'f':
         strcpy(flnm,argv[2]);
         printf("got %s for filename \n",flnm);
         argc--;argv++;
         break;
 default:
   printf("%s: unknown arg %s-- not processing any more args\n",
      argv[0],argv[1]);
 }
argc--;argv++;
}


printf("values for run:(pathLength=%d,replications=%d,t0=%d,stochasticPathLength=%d)\n",*pathLength,*replications,*t0,*stochasticPathLength);


}

void fPrintMathDbl(FILE * file,int length,double * matrix,char *  matrixName)
{
int i;
fprintf(file,"%s={",matrixName);
for(i=0;(i<length-1);i++){
fprintf(file,"%30.20f,",matrix[i]);}
fprintf(file,"%30.20f};\n",matrix[length-1]);
}
void fPrintMathInt(FILE * file,int length,int * matrix,char *  matrixName)
{
int i;
fprintf(file,"%s={",matrixName);
for(i=0;(i<length-1);i++){
fprintf(file,"%d,",matrix[i]);}
fprintf(file,"%d};\n",matrix[length-1]);
}

