(*
Needs["Statistics`ContinuousDistributions`"]
Needs["Statistics`MultinormalDistribution`"]
*)
(*linear model one lag one lead*)
julSubs={eps->0,g->0.049};stochSubs={eps->0.5,g->0.6};
juillardModel={
ey[t],
pdot[t]  - (0.414 pdot[t+1] + (1 - 0.414)* 
		 pdot[t-1] + 0.196 (g^2/(g-y[t]) - g) + 
		 0.276(g^2/(g - y[t-1]) - g)) + eps *pdot[t+1] ,
rr[t]- (rs[t] - 0.414 pdot[t+1] - (1-0.414)pdot[t-1]),
rs[t] - (3 pdot[t]+ y[t]),
y[t] - (0.304 y[t-1] - 0.98 rr[t] - 0.315 rr[t-1] - ey[t-1])
}/.stochSubs;
Print["solving  sseqns for ey,pdot,rr,rs,y"];
juillardEqns=Thread[(juillardModel)==0];
sseqns=Simplify[juillardEqns/.
  {ey[_]->ey,pdot[_]->pdot,rr[_]->rr,rs[_]->rs,y[_]->y}];
{lsome,some}=(Solve[sseqns,{ey,pdot,rr,rs,y}]//Simplify);
rsome=(Reduce[sseqns,{ey,pdot,rr,rs,y}]//Simplify);


modelFunctionName[juillardEqns]="julMod";
modelInfo[juillardEqns]=
  "juillard example from paper describing stack";
modelDataInfo[juillardEqns]="made up data from normal dist";
modelData[juillardEqns]=Table[RandomVariate[
  MultinormalDistribution[{0,0,0,0,0},
    IdentityMatrix[5]]],{50}];
modelShockInfo[juillardEqns]="made up data from normal dist";
modelShocks[juillardEqns]=
 Table[RandomVariate[MultinormalDistribution[{0,0,0,0,0},
   IdentityMatrix[5]]],{30}];
modelDefaultParameters[juillardEqns]={0.000,0.049};



modelFpGuess[juillardEqns]=({ey,pdot,rr,rs,y}/.some);
