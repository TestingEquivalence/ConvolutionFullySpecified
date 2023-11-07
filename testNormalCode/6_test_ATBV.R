source("testStatisticNormal.R")
source("asymptoticTestBootstrapVariance.R")

parameter=list()
parameter$alpha=0.05
set.seed(10071977)
parameter$x=rnorm(100)
parameter$h=0.1
parameter$nSimulation=1000
parameter$f=testStatisticNormalU

# bootstrapSD(parameter)
testStatisticNormalU(parameter$x,parameter$h)
asymptoticTestBootstrapVariance(parameter)

parameter$f=testStatisticNormalFull
testStatisticNormalFull(parameter$x,parameter$h)
asymptoticTestBootstrapVariance(parameter)

