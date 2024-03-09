source("testStatisticUniform.R")
source("asymptoticTestBootstrapVariance.R")
source("simulation/size.R")
source("simulation/power.R")
source("distributions/alternatives.R")
source("distributions/mixedDistribution.R")
library(tictoc)


# study the test power at the uniform distribution on [0,1]
sampleSize=100
vh=seq(0.1,1, by=0.1)
resPower=list()

for (h in vh){
  parameter=list()
  parameter$alpha=0.05
  parameter$h=h
  parameter$nSimulation=200
  parameter$f=testStatisticUniformU
  
  test<-function(x){
    parameter$x=x
    tres=asymptoticTestBootstrapVariance(parameter)
    return(tres$min_eps)
  }
  
  # simulate power at uniform distribution
  
  resPower[[paste0("h",h)]]=simulatePowerAtPoint(test, sampleSize, nSimulation = 1000, rDistr = runif)
}

write.csv(resPower,"resPower_ATBV_200.csv")

# simulate power at boundary point

lrdg=listRDG()

parameter=list()
parameter$alpha=0.05
parameter$h=0.35
parameter$nSimulation=1000
parameter$f=testStatisticUniformU

rf=lrdg[[1]]
h=0.35
vdst=c()
# tic("start U")

for (i in c(1:100)){
  parameter$x=rf(10000)
  vdst=c(vdst,testStatisticUniformU(parameter$x, h))
}
mean(vdst)
sd(vdst)
# toc()
#testStatisticUniformFull(parameter$x,h)


