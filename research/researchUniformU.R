source("testStatisticUniform.R")
source("asymptoticTestBootstrapVariance.R")
source("simulation/size.R")
source("simulation/power.R")
source("distributions/alternatives.R")
source("distributions/mixedDistribution.R")

library(tictoc)


# study the test power at the uniform distribution on [0,1]
sampleSize=100
h=0.34

parameter=list()
parameter$alpha=0.05
parameter$h=h
parameter$nSimulation=1000
parameter$f=testStatisticUniformU

test<-function(x){
  parameter$x=x
  tres=asymptoticTestBootstrapVariance(parameter)
  return(tres$min_eps)
}

# simulate power at uniform distribution

res=simulatePowerAtPoint(test, sampleSize, nSimulation = 1000, rDistr = runif)
write.csv(res,"size.csv")

# simulate power at boundary point

lrdg=listRDG()

parameter=list()
parameter$alpha=0.05
parameter$h=0.36
parameter$nSimulation=1000
parameter$f=testStatisticUniformU

rf=lrdg[[1]]
parameter$x=rf(10000)
h=0.36
tic("start U")
testStatisticUniformU(parameter$x, h)
toc()
#testStatisticUniformFull(parameter$x,h)
