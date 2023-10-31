source("testStatisticUniform.R")
source("asymptoticTestBootstrapVariance.R")

parameter=list()
parameter$alpha=0.05
set.seed(10071977)
parameter$x=runif(100)
parameter$h=0.1
parameter$nSimulation=1000
parameter$f=testStatisticUniformU

# bootstrapSD(parameter)
testStatisticUniformU(parameter$x,parameter$h)
asymptoticTestBootstrapVariance(parameter)

parameter$f=testStatisticUniformFull
testStatisticUniformFull(parameter$x,parameter$h)
asymptoticTestBootstrapVariance(parameter)
