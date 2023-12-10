source("testStatisticUniform.R")
source("asymptoticTestBootstrapVariance.R")
source("simulation/size.R")


# study the test power at the uniform distribution on [0,1]
sampleSize=100
h=0.1

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

res=simulatePowerAtUniform(test, sampleSize, nSimulation = 1000)
write.csv(res,"size.csv")

