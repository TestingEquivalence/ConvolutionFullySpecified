source("testStatisticUniform.R")
source("BootstrapTestTPercentile.R")

parameter=list()
parameter$alpha=0.05
set.seed(10071977)
parameter$x=runif(100)
parameter$h=0.2
parameter$nSimPercentileTBootstrap=1000
parameter$f=testStatisticUniformU

# bootstrapSD(parameter)
parameter$nSimulation=200
asymptoticTestBootstrapVariance(parameter)
parameter$nSimulation=50
parameter$nSimPercentileTBootstrap=200
tPercentileBootstrapTest(parameter)



parameter$f=testStatisticUniformFull
parameter$nSimulation=200
asymptoticTestBootstrapVariance(parameter)
parameter$nSimulation=50
parameter$nSimPercentileTBootstrap=200
tPercentileBootstrapTest(parameter)

