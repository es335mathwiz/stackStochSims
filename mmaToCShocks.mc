/*<*dvalsInfo*>*/
void <*functionName*>Shocks(int t,double * vectorOfVals)
{
int i;
#include "<*outFileString <> "ShocksForInclude.h"*>"/*dstr;*/
for(i=0;i<<*shocksCols*>;i++)vectorOfVals[i]=0;
for(i=0;i<<*modelNumberOfEquations-numbExog*>;i++)vectorOfVals[i]=theShocks[t][i];
}
