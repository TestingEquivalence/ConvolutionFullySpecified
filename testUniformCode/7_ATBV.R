source("testStatisticUniform.R")
source("asymptoticTestBootstrapVariance.R")

parameter=list()
parameter$alpha=0.05
#set.seed(10071977)
parameter$x=runif(1000)
parameter$h=0.1
parameter$nSimulation=200
parameter$f=testStatisticUniformU

# U statistic
testStatisticUniformU(parameter$x,parameter$h)
asymptoticTestBootstrapVariance(parameter)

# Full statistic
parameter$f=testStatisticUniformFull
testStatisticUniformFull(parameter$x,parameter$h)
asymptoticTestBootstrapVariance(parameter)

