/*Mathematica Creation Date<*Date[]*>*/
/*<*modelCreationInfo*>*/
#include "runItExternalDefs.h"
#include "distStochSims.h"
main(int argc, char * argv[])
{
#include "runItInvariantLocalDefs.h"
#include "run<*outFileString*>LocalDefs.h"
#include "runItInvariantMpiDefs.h"
printf("$Id: mpiRunIt.mc,v 1.1 2001/06/19 19:49:23 m1gsa00 Exp m1gsa00 $\n");



  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numberOfProcesses);
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  MPI_Get_processor_name(processorName, &nameLength);

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
&varthetaC,&varthetaCj,&varthetaCi,
&varthetaZstar,&varthetaZstarj,&varthetaZstari
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
printf("after storage allocations\n totalTime=%f,
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
allocMa50(numberOfEquations,lags,leads,pathLength,pathLength*MAXELEMENTS,
		  &ma50bdIptru,
		  &ma50bdIptrl,
		  &ma50bdIrnf,
		  &ma50bdFact,
		  &ma50bdIq,
		  &ma50bdJob);
allocMa50(numberOfEquations,lags,leads,1,MAXELEMENTS,
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
&<*functionName*>PathQ,&<*functionName*>ZeroPathQ);
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
	  <*functionName*>DataVals[(i+pathLength+t0)*numberOfEquations+j];
  }}








/*
<*functionName*>PeriodicPointGuesser(parameters,1,<*functionName*>FP);

printf("initiating FP solution computation\n");
FPnewt(&numberOfEquations,&lags,&leads,
<*functionName*>,<*functionName*>Derivative,parameters,
<*functionName*>FP,
fmats,fmatsj,fmatsi,
smats,smatsj,smatsi,
&maxNumberElements,
failedQ,intControlParameters,doubleControlParameters,
intOutputInfo, doubleOutputInfo);
printf("computed FP solution\n");
*/
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after fixed point computation\n totalTime=%f,userSystemTime=%f,systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));






printf("generating perm vec\n");
allocGenerateDraws(1,stochasticPathLength,replications,
&<*functionName*>PermVec);
 generateDraws(1,(stochasticPathLength),replications,numberOfShocks,<*functionName*>PermVec);
printf("done generating perm vec\n");

/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after generating draws\n totalTime=%f,userSystemTime=%f,
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));








/*compute asymptotic Q constraint*/
if(!useIdentityQ){
altComputeAsymptoticQMatrix(
&numberOfEquations,&lags,&leads,
<*functionName*>,<*functionName*>Derivative,parameters,
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
cstar,cstarj,cstari,
AMqMatrix,AMqMatrixj,AMqMatrixi,
&auxInit,&qRows,rootr,rooti,
AMbMatrix,AMbMatrixj,AMbMatrixi,
phiInvMat,phiInvMatj,phiInvMati,
fmat,fmatj,fmati,
varthetaC,varthetaCj,varthetaCi,
varthetaZstar,varthetaZstarj,varthetaZstari,
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
printf("after computing Q,max elems=%d\n totalTime=%f,userSystemTime=%f,
systemTime=%f\n",spaMaxNumberElements,*totalTime,*userSystemTime,*(userSystemTime+1));
}

if(qRows <numberOfEquations*leads){
printf("qmatrix has %d rows, need %d so using identity matrix terminal condition\n",qRows,numberOfEquations*leads);
altComputeAsymptoticIMatrix(
&numberOfEquations,&lags,&leads,
AMqMatrix,AMqMatrixj,AMqMatrixi,
failedQ
);
}

/*
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
		<*functionName*>ZeroPathQ+(i *numberOfEquations),<*functionName*>FP,
		AMbMatrix,AMbMatrixj,AMbMatrixi,
<*functionName*>ZeroPathQ+((i * numberOfEquations)+(numberOfEquations*lags)));
for(j=0;j<numberOfEquations;j++){
<*functionName*>ZeroPathQ[((i * numberOfEquations)+(numberOfEquations*lags))+j]=
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



/*begin biiiiiiiiiig MPI paste*/
  if (myRank == 0)
    {
      startwtime = MPI_Wtime();
      printf("Process #%d: %d replications.\n", myRank, replications);
    }

  printf("Process %d has been started on host %s\n", myRank, processorName);

  pathQLength = replications * numberOfEquations *
    (lags + leads + pathLength + stochasticPathLength);
  failedQLength = replications;
  buildResultType(<*functionName*>PathQ, failedQ, &completedDraw, pathQLength,
		  failedQLength, &resultMessageType);

  if ((myRank == 0) && (replications < numberOfProcesses - 1))
    {
      printf("Process #%d: Too many processes for the number of replications. Killing some 
processes.\n",
	     myRank);
      for (i = replications + 1; i < numberOfProcesses; i++)
	{
	  sendHaltMessage(i);
	}
      numberOfProcesses = replications + 1;
    }

  if (myRank == 0)
    {
      printf("Process #0: pid = %d\n", getpid());
      for (newDraw = 0; newDraw < numberOfProcesses - 1; newDraw++)
	{
	  destination = newDraw + 1;
	  sendDataMessage(destination, &newDraw);
	}

      for (numberOfCompletedDraws = 0; numberOfCompletedDraws < replications;
	   numberOfCompletedDraws++)
	{
	  printf("Process #0: About to start waiting for message\n"); fflush(stdout);
	  MPI_Recv(<*functionName*>PathQ, 1, resultMessageType, MPI_ANY_SOURCE,
		   RESULT_MSG_TAG, MPI_COMM_WORLD, &status);
	  printf("Process #0: Received message\n"); fflush(stdout);
	  source = status.MPI_SOURCE;
	  if (status.MPI_ERROR != 0)
	    error(myRank, source, status.MPI_TAG, status.MPI_ERROR);
	  printf("Process #0 received results of replication %d from process #%d.\n",
		 completedDraw, source); fflush(stdout);

	  /**************
	  writeOutput(flnm, completedDraw, <*functionName*>PathQ, failedQ, pathQLength,
		      failedQLength, pathLength, t0, stochasticPathLength,
		      replications, <*functionName*>PermVec, <*functionName*>DataVals,
		      <*functionName*>ShockVals, lags, leads, numberOfEquations, numberOfShocks,
		      numberOfDataValues);
		      **************/
	  if (numberOfCompletedDraws < replications - numberOfProcesses + 1)
	    {
	      destination = source;
	      sendDataMessage(destination, &newDraw);
	      newDraw++;
	    }
	  else /*** tell other process that we're done ***/
	    {
	      destination = source;
	      sendHaltMessage(destination);
	    }
	}
      printf("Process #%d: All replications have completed.\n", myRank); fflush(stdout);
      endwtime = MPI_Wtime();
      printf("Process #%d: Wall clock time = %f.\n", myRank, endwtime - startwtime);
      fflush(stdout);
    }
  else /*** myRank not 0 ***/
    {
      int halt = 0;

      printf("Process #%d: pid = %d\n", myRank, getpid());

      while (!halt)
	{
	  source = 0;
	  printf("Process #%d: About to start waiting for message\n", myRank); fflush(stdout);
	  MPI_Recv(buffer, BUFFER_SIZE, MPI_PACKED, source, MPI_ANY_TAG, MPI_COMM_WORLD,
		   &status);
	  printf("Process #%d: Received message\n", myRank); fflush(stdout);
	  printf("Process #%d: About to check for errors. source = %d, tag = %d, status. MPIERROR = 
%d.\n",
		 myRank, source, tag, status.MPI_ERROR); fflush(stdout);
	  tag = status.MPI_TAG;
	  if (status.MPI_ERROR != 0)
	    error(myRank, source, tag, status.MPI_ERROR);
	  fflush(stderr);
	  printf("Process #%d: Completed error check.\n", myRank); fflush(stdout);

	  if (tag == HALT_MSG_TAG)
	    halt = 1;
	  else
	    {
	      position = 0;
	      MPI_Unpack(buffer, BUFFER_SIZE, &position, &newDraw, 1, MPI_INT,
			 MPI_COMM_WORLD);

	      sprintf(outFileName, "%s%d", flnm, newDraw);
	      outFile=fopen(outFileName,"w");

	      /*set terminal time and call stochSim*/
	      tf=t0+stochasticPathLength-1;

	      printf("Process #%d: saving values for variable in file named 
%s\n",myRank,outFileName);
	      fprintf(outFile,"<*functionName*>RunParams={%d,%d,%d,%d,%d,%d,%d};\n",
		      numberOfEquations,lags,leads,
		      pathLength,t0,stochasticPathLength,replications);
	      fPrintMathInt(outFile,replications * (stochasticPathLength),
			    <*functionName*>PermVec,"<*functionName*>PermVec");
	      /************************  these generate too much output for frbus
	      
fPrintMathDbl(outFile,(numberOfEquations*numberOfDataValues),<*functionName*>DataVals,"<*functionName*>Data");
	      
fPrintMathDbl(outFile,(numberOfEquations*numberOfShocks),<*functionName*>ShockVals,"<*functionName*>Shocks");
/*end biiiiiiiiiig MPI paste*/

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


fPrintMathDbl(outFile,(numberOfEquations*(leads+pathLength+lags)),
      <*functionName*>ZeroPathQ,"<*functionName*>ZeroShockResults");
fPrintMathInt(outFile,1,
failedQ,"<*functionName*>ZeroShocksFailedQ");
/*time used so far?*/
*totalTime=dtime_(userSystemTime);
printf("after using Q matrix\ntotalTime=%f,userSystemTime=%f,
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));
*cmpma50bdJob=1;
diststochSim(&numberOfEquations,&lags,&leads,&pathLength,
<*functionName*>,<*functionName*>Derivative,parameters,
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
cmpma50bdIptru,newDraw,outFile
);
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
printf("after using Q matrix\ntotalTime=%f,userSystemTime=%f,
systemTime=%f\n",*totalTime,*userSystemTime,*(userSystemTime+1));
	      writeOutput(outFile, newDraw, <*functionName*>PathQ, failedQ, pathQLength,
			  failedQLength, pathLength, t0, stochasticPathLength,
			  replications, <*functionName*>PermVec, <*functionName*>DataVals,
			  <*functionName*>ShockVals, lags, leads, numberOfEquations, numberOfShocks,
			  numberOfDataValues);

	      printf("Process #%d: Finished stochSim().\n", myRank); fflush(stdout);

	      destination = 0;
	      tag = RESULT_MSG_TAG;
	      completedDraw = newDraw;
	      printf("Process #%d sending replication %d results to process #0.\n",
		     myRank, completedDraw);
	      MPI_Send(<*functionName*>PathQ, 1, resultMessageType, destination,
		       tag, MPI_COMM_WORLD);
	      fclose(outFile);
	    }
}}

  MPI_Finalize();






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
&varthetaC,&varthetaCj,&varthetaCi,
&varthetaZstar,&varthetaZstarj,&varthetaZstari
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
 
 
#include "runItOther.h"

