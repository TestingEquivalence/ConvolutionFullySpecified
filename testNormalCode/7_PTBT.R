source("testStatisticNormal.R")
source("BootstrapTestTPercentile.R")

parameter=list()
parameter$alpha=0.05
set.seed(10071977)
parameter$x=rnorm(100)
parameter$h=0.3


# U statistic
parameter$f=testStatisticNormalU
parameter$nSimulation=200
asymptoticTestBootstrapVariance(parameter)

parameter$nSimulation=50
parameter$nSimPercentileTBootstrap=200
tPercentileBootstrapTest(parameter)


# full statistic
parameter$f=testStatisticNormalFull
parameter$nSimulation=200
asymptoticTestBootstrapVariance(parameter)

parameter$nSimulation=50
parameter$nSimPercentileTBootstrap=200
tPercentileBootstrapTest(parameter)

