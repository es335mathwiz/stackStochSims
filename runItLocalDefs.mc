
/*Mathematica Creation Date<*Date[]*>*/
#define SPAMAXELEMENTS <*15*(modelNumberOfEquations)^2*>
#define MAXELEMENTS <*25*(modelNumberOfEquations)^2*>

int  maxNumberElements=MAXELEMENTS;
int  spaMaxNumberElements=SPAMAXELEMENTS;
void <*functionName*>(double * xvec,double * pvec,double * shock,
double * alhs,
int * jalhs,int * ialhs,int * alphas,double * linPt
);
void <*functionName*>Derivative(double * xvec,double * pvec,
double * alhs,
int * jalhs,
int * ialhs);
void <*functionName*>ExogH(double * pvec,
double * alhs,
int * jalhs,
int * ialhs);
/*model specific names and data*/
char * namesArray[] =  
<*("\""<>ToString[#]<>"\"")& /@ allv *>;
char * paramNamesArray[] =  
<*("\""<>ToString[#]<>"\"")& /@ allcoeffs *>;
double parameters[]=
<*defaultParams*>;
int <*functionName*>exogQ[]=
<*exogQ*>;

int * <*functionName*>PermVec;
double * <*functionName*>ZeroShock;
double * <*functionName*>ShockVals;
double * <*functionName*>DataVals;
double * <*functionName*>FP;
double * <*functionName*>Intercept;
double * <*functionName*>EasyPathQ;
double * <*functionName*>TargetPathQ;
double * <*functionName*>PathQ;
double * <*functionName*>ZeroPathQ;
